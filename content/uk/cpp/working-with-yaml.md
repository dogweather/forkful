---
title:                "Робота з YAML"
date:                  2024-01-19
simple_title:         "Робота з YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це таке & Чому?

YAML — це людино-читабельний формат для конфігураційних файлів. Програмісти використовують його, щоб легко серіалізувати дані для зберігання або міжпроцесної комунікації.

## Як це зробити:

У C++ робота з YAML вимагає сторонньої бібліотеки, наприклад, `yaml-cpp`. Ось як ви можете читати та писати YAML файлы:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>

// Читаємо YAML
void ReadYAML(const std::string &filename) {
    YAML::Node config = YAML::LoadFile(filename);
    if (config["name"]) {
        std::cout << "Name: " << config["name"].as<std::string>() << std::endl;
    }
}

// Пишемо YAML
void WriteYAML(const std::string &filename) {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "name" << YAML::Value << "Viktor";
    out << YAML::EndMap;

    std::ofstream fout(filename);
    fout << out.c_str();
}

int main() {
    const std::string filename = "example.yaml";
    
    WriteYAML(filename);
    ReadYAML(filename);
    
    return 0;
}
```

Припустимо, ви отримаєте вивід: `Name: Viktor`.

## Поглиблений розбір:

YAML виник у 2001 році як більш читабельна альтернатива XML. Він дозволяє вкладеність, займає менше місця і простий у використанні. Альтернативи YAML — це JSON і XML, але YAML часто використовується завдяки його простоті. Бібліотека `yaml-cpp` є найпоширенішою для C++, вона об'єктно-орієнтована і підтримує серіалізацію/десеріалізацію складних даних.

## Дивіться також:

- Офіційний сайт YAML: https://yaml.org/
- `yaml-cpp` GitHub: https://github.com/jbeder/yaml-cpp
- YAML специфікація: https://yaml.org/spec/1.2/spec.html
- YAML в порівнянні з JSON і XML: https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json-when-to-prefer-one-over-the-other
