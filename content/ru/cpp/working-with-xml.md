---
title:                "Работа с XML"
date:                  2024-01-29T00:04:59.588552-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML предполагает анализ, создание и манипулирование данными XML (eXtensible Markup Language, Расширяемый язык разметки). Программисты используют XML для обработки структурированного обмена данными, конфигурации и прочего, благодаря его нейтральности к платформам.

## Как это сделать:
Вот простой способ разбора XML с помощью библиотеки TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Привет, мир!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

Пример вывода:

```
Привет, мир!
```

И вот как можно создать XML файл:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* message = doc.NewElement("message");
    message->SetText("Привет, мир!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

Это создаст XML файл `output.xml` со следующим содержимым:

```xml
<?xml version="1.0"?>
<root>
    <message>Привет, мир!</message>
</root>
```

## Глубокое погружение
XML был ключевым в веб-сервисах и хранении данных с конца 90-х. Хотя сейчас для конфигурации и взаимодействия чаще используются JSON и YAML, XML все еще играет важную роль во многих корпоративных системах. Анализ XML в C++ может показаться старомодным из-за ручного разбора DOM/SAX. К счастью, библиотеки вроде TinyXML-2 упрощают этот процесс. C++ не имеет встроенной поддержки XML; библиотеки вроде TinyXML-2, pugixml или Xerces берут на себя сложные аспекты работы.

## Смотрите также
- Документация TinyXML-2: https://leethomason.github.io/tinyxml2/
- Библиотека pugixml: https://pugixml.org/
- Парсер Xerces-C++: https://xerces.apache.org/xerces-c/
- Спецификация XML от W3C: https://www.w3.org/XML/
