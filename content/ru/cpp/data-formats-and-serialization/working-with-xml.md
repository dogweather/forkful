---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:59.588552-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0441\
  \u043F\u043E\u0441\u043E\u0431 \u0440\u0430\u0437\u0431\u043E\u0440\u0430 XML \u0441\
  \ \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0438 TinyXML-2."
lastmod: '2024-03-13T22:44:45.651088-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0441\u043F\
  \u043E\u0441\u043E\u0431 \u0440\u0430\u0437\u0431\u043E\u0440\u0430 XML \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0438 TinyXML-2."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

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
