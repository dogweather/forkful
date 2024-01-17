---
title:                "Робота з json"
html_title:           "Rust: Робота з json"
simple_title:         "Робота з json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-json.md"
---

{{< edit_this_page >}}

# Що і чому?

Робота з JSON - це процес кодування та декодування даних у форматі JSON (JavaScript Object Notation). JSON є одним з найпоширеніших форматів для обміну даними в Інтернеті і знаходить своє застосування в програмуванні. Швидкість та простота зробили JSON вибором для більшості програмістів.

# Як це зробити:

```Rust 
use serde_json::Value; 

fn main() { 
    let data = r#"{ "name": "John", "age": 30, "city": "New York" }"#; 
   
    let parsed:Value = serde_json::from_str(data).unwrap(); 
   
    let name = parsed["name"].as_str().unwrap(); 
    let age = parsed["age"].as_u64().unwrap(); 
    let city = parsed["city"].as_str().unwrap(); 
   
    println!("Name: {}", name); 
    println!("Age: {}", age); 
    println!("City: {}", city); 
} 
```

В цьому прикладі ми використовуємо бібліотеку serde_json, щоб здійснити розбір рядка з даними у форматі JSON. Після цього ми можемо отримати доступ до конкретних полів даних, використовуючи індексацію. Зверніть увагу, що ми використовуємо різні методи, наприклад as_str () та as_u64 (), для отримання значення відповідного типу.

# Глибокий погляд:

JSON був створений у 1999 році та став популярним з форматуванням даних у JavaScript. З тих пір він став стандартом у веб-розробці і знайшов застосування у багатьох інших мовах програмування, включаючи Rust. В невеликому розмірі файлів та можливості кодувати різні типи даних, наприклад рядки, числа, масиви та об'єкти, полягає сила та популярність JSON. Альтернативами для роботи з JSON є XML та CSV, але вони мають свої обмеження та складніше сприймаються програмістами.

В Rust, є кілька бібліотек для роботи з JSON, таких як serde_json, json-rust та rustc-serialize, кожна з яких має свої переваги та недоліки. Serde_json є найчастіше використовуваною бібліотекою, оскільки її структури даних можуть бути використані для автоматичного створення структур даних у Rust.

# Дивіться також:

- [Офіційна документація serde_json](https://docs.serde.rs/serde_json/index.html)
- [Стаття про використання serde_json у Rust проекті](https://www.ameyalokare.com/rust/2017/09/02/rust-serdejson.html)
- [Порівняння з іншими бібліотеками JSON у Rust](https://github.com/serde-rs/json-benchmark)