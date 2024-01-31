---
title:                "Работа с YAML"
date:                  2024-01-29T00:04:39.003385-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с YAML включает в себя разбор и генерацию данных в удобном для человека формате YAML Ain't Markup Language. Программисты используют его для файлов конфигурации, сериализации данных и настроек приложений из-за его читабельности и простоты.

## Как:

Поддержка YAML не встроена в C++. Вам понадобится библиотека, например, `yaml-cpp`. Вот как разобрать простой файл YAML:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("config.yaml");
    YAML::Node config = YAML::Load(file);
    
    std::string username = config["user"]["name"].as<std::string>();
    int age = config["user"]["age"].as<int>();
    
    std::cout << "Имя: " << username << ", Возраст: " << age << std::endl;
    return 0;
}
```

Предполагая, что `config.yaml` это:
```
user:
  name: John Doe
  age: 30
```

Вывод:
```
Имя: John Doe, Возраст: 30
```

## Подробнее

YAML был впервые представлен в 2001 году как стандарт сериализации данных, удобочитаемый для человека. Хотя JSON и XML являются распространенными альтернативами, минимальный синтаксис YAML сделал его популярным для файлов конфигурации. Библиотеки, такие как `yaml-cpp`, обрабатывают разбор и вывод данных YAML, представляя их в структурах, подобных картам и последовательностям, аналогично объектам и массивам JSON.

## Смотрите также

- Спецификация YAML 1.2: https://yaml.org/spec/1.2/spec.html
- Репозиторий yaml-cpp на GitHub: https://github.com/jbeder/yaml-cpp
- Введение в YAML: https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started
