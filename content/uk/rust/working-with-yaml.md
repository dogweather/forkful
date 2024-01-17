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

# Що і Чому?

YAML - це формат даних, що використовується для зберігання інформації у структурованому вигляді. Програмісти використовують YAML для зручного представлення даних та легшого взаємодії з ними.

## Як це зробити:

```Rust
use yaml_rust::YamlLoader;

let data = "
name: Іван
age: 25";

let docs = YamlLoader::load_from_str(data).unwrap();
let ivan = &docs[0];
println!("Ім'я: {}", ivan["name"]);
println!("Вік: {}", ivan["age"]);
```

Вище наведений код демонструє, як можна прочитати дані у форматі YAML та отримати до них доступ за допомогою мови програмування Rust.

## Глибше погляньмо:

YAML був створений у 2001 році як альтернатива формату XML. У порівнянні з XML, YAML є більш зручним та простим у використанні, оскільки не вимагає тегів та зворотних слешів. Окрім того, YAML дозволяє створювати читабельні структури даних у вигляді списків та ключ-значення.

Існують інші альтернативні формати даних, такі як JSON та TOML. Однак, YAML використовується, коли потрібно зберегти дані у більш інтуїтивно зрозумілому форматі для людей.

Реалізація роботи з YAML у Rust здійснюється за допомогою бібліотеки ```yaml_rust```, яка надає зручний API для роботи зі структурами даних у форматі YAML.

## Дивись також:

- [Офіційна документація з мови програмування Rust](https://www.rust-lang.org/uk)
- [YAML формат даних](https://yaml.org/)
- [JSON, TOML та інші формати даних](https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats)