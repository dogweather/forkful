---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:39.003385-07:00
description: "\u041A\u0430\u043A: \u041F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\
  \u0430 YAML \u043D\u0435 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u0430 \u0432\
  \ C++. \u0412\u0430\u043C \u043F\u043E\u043D\u0430\u0434\u043E\u0431\u0438\u0442\
  \u0441\u044F \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430, \u043D\
  \u0430\u043F\u0440\u0438\u043C\u0435\u0440, `yaml-cpp`. \u0412\u043E\u0442 \u043A\
  \u0430\u043A \u0440\u0430\u0437\u043E\u0431\u0440\u0430\u0442\u044C \u043F\u0440\
  \u043E\u0441\u0442\u043E\u0439 \u0444\u0430\u0439\u043B YAML."
lastmod: '2024-03-13T22:44:45.644371-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0430 YAML \u043D\u0435\
  \ \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u0430 \u0432 C++."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
