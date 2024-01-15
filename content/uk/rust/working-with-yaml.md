---
title:                "Робота з yaml"
html_title:           "Rust: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

ЯМЛ (YAML) - це формат, що дозволяє представляти дані в зручному для людей форматі, що одночасно зберігає структуру. Використовуючи Раст (Rust), ви можете легко зчитати та зберегти дані у форматі ЯМЛ, що робить його корисним для збереження конфігураційних файлів, налаштувань програм та багатьох інших сценаріїв.

## Як це зробити

Щоб використати ЯМЛ у своїй програмі, спочатку необхідно додати залежність у файл Cargo.toml:

```Rust
[dependencies]
yaml = "0.6.3"
```

Далі, необхідно імпортувати бібліотеку `yaml` у свою програму:

```Rust
extern crate yaml;
use yaml::{YamlLoader, Yaml};
```

І, нарешті, можна зчитати та зберегти ЯМЛ дані:

```Rust
let data = "
students:
  - name: John
    age: 19
  - name: Jane
    age: 20
";

let yaml_data = YamlLoader::load_from_str(data).unwrap();
let students = yaml_data[0]["students"].clone();

// Записати дані у форматі ЯМЛ
let yaml = Yaml::String("Hello, World!".to_string());
yaml.write_to_file("output.yml").unwrap();

// Вивести дані
println!("Студенти:");
for student in students {
  let name = student["name"].as_str().unwrap();
  let age = student["age"].as_i64().unwrap();

  println!("{} - {} років", name, age);
}
```

В результаті отримаємо наступний вивід:

```
Студенти:
John - 19 років
Jane - 20 років
```

## Поглиблене вивчення

ЯМЛ має багато особливостей та можливостей, які зроблять роботу з цим форматом ще зручнішою. Наприклад, ви можете використовувати табличний вигляд для представлення даних, а також вкладені структури та масиви для складних структур даних. Ви можете дізнатися більше про ЯМЛ та його можливості на [офіційному сайті](https://yaml.org/).

## Дивіться також

- [Офіційний сайт ЯМЛ](https://yaml.org/)
- [Документація бібліотеки yaml для Раст](https://docs.rs/yaml/0.6.3/yaml/)