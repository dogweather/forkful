---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:53.839018-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 YAML \u0443 C++\
  \ \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u043C \u0432\u0438\u0431\
  \u043E\u0440\u043E\u043C \u0454 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0430 `yaml-cpp`. \u0421\u043F\u0435\u0440\u0448\u0443, \u043F\u0435\u0440\
  \u0435\u043A\u043E\u043D\u0430\u0439\u0442\u0435\u0441\u044F, \u0449\u043E \u0443\
  \ \u0432\u0430\u0441 \u0432\u0441\u0442\u0430\u043D\u043E\u0432\u043B\u0435\u043D\
  \u043E `yaml-cpp` \u0442\u0430 \u043F\u0440\u0430\u0432\u0438\u043B\u044C\u043D\u043E\
  \u2026"
lastmod: '2024-03-13T22:44:49.882275-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 YAML \u0443\
  \ C++ \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u043C \u0432\u0438\u0431\
  \u043E\u0440\u043E\u043C \u0454 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0430 `yaml-cpp`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

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
