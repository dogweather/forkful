---
title:                "Робота з YAML"
aliases: - /uk/cpp/working-with-yaml.md
date:                  2024-02-03T19:24:53.839018-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

YAML, що означає YAML Ain't Markup Language, це формат серіалізації даних, зручний для читання людиною. Програмісти використовують його для файлів конфігурації, вивантаження даних та зберігання ієрархічних даних через його читабельність та легкість у сприйнятті синтаксису в порівнянні з XML або JSON.

## Як це зробити:

Для роботи з YAML у C++ популярним вибором є бібліотека `yaml-cpp`. Спершу, переконайтеся, що у вас встановлено `yaml-cpp` та правильно зв'язано з вашим проектом на C++.

**Читання файлу YAML:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Назва: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Припустимо, що `config.yaml` виглядає так:

```yaml
title: "Приклад YAML"
```

Виконання вищенаведеного коду на C++ дало б такий результат:

```
Назва: Приклад YAML
```

**Запис у файл YAML:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Приклад YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

Цей код створить `output.yaml` з вмістом:

```yaml
title: Приклад YAML
```

Ці приклади слугують базовим вступом до читання з файлів YAML та запису в них у C++ за допомогою бібліотеки `yaml-cpp`. Для більш складних структур та випадків використання, досліджуйте документацію `yaml-cpp`, щоб ознайомитися з особливостями, такими як послідовності, теги та більш продвинуті техніки серіалізації та десеріалізації.
