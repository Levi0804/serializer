use std::collections::hash_map::HashMap;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct IntField<'a> {
    name:Option< &'a str>,
    min: Option<i32>,
    max: Option<i32>,
    bits:Option<i32>,
    always_present: Option<bool>,
}   
    
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct BooleanField<'a> {
    name: Option<&'a str>,
    bits: Option<i32>,
}
    
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct BytesField<'a> {
    name:Option< &'a str>,
    max: Option<i32>,
    bits:Option<i32>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Field<'a> {
    Int(IntField<'a>),
    Boolean(BooleanField<'a>),
    Bytes(BytesField<'a>),
}

#[derive(Debug)]
enum Value<'a> {
    Buffer(&'a [u8]),
    Int(i32),
    Boolean(bool),
}

#[allow(unused)]
#[derive(Debug)]
struct Schema<'a> {
    fields: Vec<Field<'a>>,
    int: i32,
    max_byte_length: i32,
    bytes: Vec<i32>,
}

impl<'a> Schema<'a> {
    fn construct(fields: &'a mut [Field<'a>]) -> Self {
        let mut max_bits = 0;
        let mut v = Vec::<Field>::new();
        for i in 0..fields.len() {
            if let Field::Int(IntField {min, max, name, ..}) = fields[i] {
                let normalized = max.unwrap() - min.unwrap();
                let bits = (f32::log2((normalized + 1) as f32)).ceil() as i32;
                max_bits += bits;
                v.push(Field::Int(
                    IntField {
                        name,
                        min,
                        bits: Some(bits),
                        max,
                        always_present: Some(bits == 0),
                    }       
                )); 
            } else if let Field::Boolean(BooleanField { name, .. }) = fields[i] {
                max_bits = max_bits + 1;
                v.push(Field::Boolean(
                    BooleanField {  
                        bits: Some(1),
                        name,
                    }   
                ));
            } else if let Field::Bytes(BytesField {name, max, ..}) = fields[i] {
                let bits = (f32::log2((max.unwrap() + 1) as f32)).ceil() as i32;
                max_bits += bits;   
                v.push(Field::Bytes(
                    BytesField {  
                        bits: Some(bits),
                        name,
                        max,
                    }
                ));
            }   
        }

        v.sort_by(|a, b| {
            if let Field::Bytes(_) = a {
                std::cmp::Ordering::Greater
            } else if let Field::Bytes(_) = b {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        }); 

        let max_byte_length = ((max_bits / 8) as f32).ceil() as i32;

        Schema {    
            fields: v,
            int: 0,
            max_byte_length,
            bytes: Vec::<i32>::new(),
        }
    }

    fn to_buffer(&self, value: HashMap<&'a str, Value>) -> Vec<u8> {
        let mut int = 0;
        let mut written_bytes = 0_i32;
        let mut bytes =  Vec::new();
        let mut buffers = Vec::<&[u8]>::new();
        let mut total_buffers_length = 0;

        for i in 0..self.fields.len() {
            if let Field::Int(IntField {name, min, bits, ..}) = self.fields[i] {
                if let Value::Int(val) = value.get(name.unwrap()).unwrap() {
                    let normalized = val - min.unwrap();
                    int |= normalized << written_bytes;
                    if let Some(v) = bits { written_bytes += v; }
                }       
            } else if let Field::Boolean(BooleanField { name,.. }) = self.fields[i] {
                if let Value::Boolean(expr) = value.get(name.unwrap()).unwrap() { 
                    int |= (if *expr { 1 } else  { 0 }) << written_bytes;      
                    written_bytes = written_bytes + 1;
                };      
            } else if let Field::Bytes(BytesField { name, bits, .. }) = self.fields[i] {
                if let Value::Buffer(buffer) = value.get(name.unwrap()).unwrap() {
                    buffers.push(*buffer);          
                    total_buffers_length = total_buffers_length + buffer.len();
                    int |= (buffer.len() as i32) << written_bytes;
                    written_bytes += bits.unwrap();
                }
            }

            while written_bytes >= 8 && int > 0 {
                bytes.push(int & 0b11111111);
                int >>= 8;
                written_bytes -= 8;
            }
        }   

        while written_bytes > 0 && int > 0 {
            bytes.push(int & 0b11111111);
            int >>= 8;
            written_bytes -= 8;
        }

        let mut byte_array = Vec::<u8>::with_capacity(bytes.len() + total_buffers_length);  
        for byte in &bytes {
            byte_array.push(*byte as u8);
        }

        for i in 0..buffers.len() {
            let buffer = buffers[i];
            for j in 0..buffer.len() {
                byte_array.insert(bytes.len() + j, buffer[j]);
            }
        }

        byte_array
    }
    
    fn from_buffer(&'a self, buffer: &'a Vec<u8>) -> HashMap<&'a str, Value> {
        let fields = &self.fields;

        let mut int = buffer[0] as i32;
        let mut read_bits = 8;
        let mut buffer_index = 1;

        let mut value: HashMap<&str, Value> = HashMap::new();

        for i in 0..fields.len() {
            let field = &fields[i];
            if let Field::Int(IntField { always_present,name, min, .. }) = field {
                if let Some(present) = always_present {
                    if *present {
                        value.insert(name.unwrap(), Value::Int(min.unwrap()));  
                        continue;
                    }
                }
            }  

            let bits = match field {
                Field::Int(f) => f.bits.unwrap(),
                Field::Bytes(f) => f.bits.unwrap(),
                Field::Boolean(f) => f.bits.unwrap(),
            };

            while read_bits < bits {
                int |= (buffer[buffer_index] as i32) << read_bits;
                buffer_index += 1;
                read_bits += 8;
            }   

            if let Field::Int(IntField { name, min, .. }) = field {
                let mask = (1 << bits) - 1;
                let val = int & mask;
                value.insert(name.unwrap(), Value::Int(val + min.unwrap()));
                int >>= bits;
                read_bits -= bits;
            } else if let Field::Boolean(BooleanField { name, .. }) = field {
                value.insert(name.unwrap(), Value::Boolean((int & 1) == 1));
                int >>= 1;
                read_bits -= 1;     
            } else if let Field::Bytes(BytesField { name, .. }) = field {
                let mask =  (1 << bits) - 1;  
                let length = (int & mask) as usize;
                int >>= bits;
                read_bits -= bits;
                value.insert(name.unwrap(), Value::Buffer(&buffer[buffer.len() - length..buffer.len()]));
            }
        }

        value       
    }
}

fn main() {
    let fields = &mut [
        Field::Int(IntField { name: Some("language"), min: Some(0), max: Some(0), bits: None, always_present: None }),
        Field::Int(IntField { name: Some("gameMode"), min: Some(0), max: Some(0), bits: None, always_present: None }),
        Field::Int(IntField { name: Some("regenChallengeDifficulty"), min: Some(0), max: Some(2), bits: None, always_present: None }),
        Field::Int(IntField { name: Some("regenChallenges"), min: Some(1), max: Some(3), bits: None, always_present: None }),
        Field::Int(IntField { name: Some("solvesPerSyllable"), min: Some(-5000), max: Some(5000), bits: None, always_present: None }),
        Field::Int(IntField { name: Some("turnDuration"), min: Some(1), max: Some(10), bits: None, always_present: None }),
        Field::Int(IntField { name: Some("startingLives"), min: Some(1), max: Some(5), bits: None, always_present: None }),
	Field::Int(IntField { name: Some("maxLives"), min: Some(1), max: Some(5), bits: None, always_present: None }),
	Field::Int(IntField { name: Some("syllableDuration"), min: Some(1), max: Some(10), bits: None, always_present: None }),
	Field::Boolean(BooleanField { name: Some("allowHyphensAndApostrophesInSyllables"), bits: None }),
	Field::Bytes(BytesField { name: Some("buffer"), max: Some(1000), bits: None }),
    ];  

    let mut hashmap: HashMap<&str, Value> = HashMap::new();
    hashmap.insert("language", Value::Int(0));
    hashmap.insert("gameMode", Value::Int(0));
    hashmap.insert("regenChallengeDifficulty", Value::Int(0));
    hashmap.insert("regenChallenges", Value::Int(3));
    hashmap.insert("solvesPerSyllable", Value::Int(1000));
    hashmap.insert("turnDuration", Value::Int(7));
    hashmap.insert("startingLives", Value::Int(2));
    hashmap.insert("maxLives", Value::Int(3));
    hashmap.insert("syllableDuration", Value::Int(2));
    hashmap.insert("allowHyphensAndApostrophesInSyllables", Value::Boolean(false));
    hashmap.insert("buffer", Value::Buffer("hello world".as_bytes()));

    let bytes = Schema::construct(fields);
    let encoded = bytes.to_buffer(hashmap);
    println!("{:?}", encoded);
    let decoded = bytes.from_buffer(&encoded); 
    println!("{:#?}", decoded);
}
