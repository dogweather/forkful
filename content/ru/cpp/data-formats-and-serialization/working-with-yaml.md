---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:39.003385-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440 \u0438 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0434\
  \u0430\u043D\u043D\u044B\u0445 \u0432 \u0443\u0434\u043E\u0431\u043D\u043E\u043C\
  \ \u0434\u043B\u044F \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u0430 \u0444\u043E\
  \u0440\u043C\u0430\u0442\u0435 YAML Ain't Markup Language. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u044E\u0442 \u0435\u0433\u043E \u0434\u043B\u044F \u0444\u0430\u0439\
  \u043B\u043E\u0432\u2026"
lastmod: '2024-03-11T00:14:19.281842-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440 \u0438 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0434\
  \u0430\u043D\u043D\u044B\u0445 \u0432 \u0443\u0434\u043E\u0431\u043D\u043E\u043C\
  \ \u0434\u043B\u044F \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u0430 \u0444\u043E\
  \u0440\u043C\u0430\u0442\u0435 YAML Ain't Markup Language. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u044E\u0442 \u0435\u0433\u043E \u0434\u043B\u044F \u0444\u0430\u0439\
  \u043B\u043E\u0432\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
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
