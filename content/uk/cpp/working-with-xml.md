---
title:                "Робота з XML"
date:                  2024-01-26T04:28:41.213560-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML означає розбір, створення та маніпуляції з даними XML (розширювана мова розмітки). Програмісти керують XML для обробки структурованого обміну даними, конфігурації та більшого завдяки його нейтральності до платформ.

## Як це зробити:
Ось простий спосіб розбору XML за допомогою бібліотеки TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Привіт, Світ!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

Приклад виводу:

```
Привіт, Світ!
```

А ось як створити XML файл:

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
    message->SetText("Привіт, Світ!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

Це генерує XML файл `output.xml` із вмістом:

```xml
<?xml version="1.0"?>
<root>
    <message>Привіт, Світ!</message>
</root>
```

## Поглиблений занурення
XML був вирішальним в веб-сервісах та зберіганні даних з кінця 90-х. Хоча JSON та YAML зараз більш поширені для конфігурації і взаємодії, XML все ще має велике значення в багатьох корпоративних системах. Розбір XML в C++ може здаватися застарілим з ручним парсингом DOM/SAX. На щастя, бібліотеки на кшталт TinyXML-2 спрощують цей процес. C++ не має вбудованої підтримки XML; бібліотеки на кшталт TinyXML-2, pugixml, або Xerces спрощують складні частини.

## Дивіться також
- Документація TinyXML-2: https://leethomason.github.io/tinyxml2/
- бібліотека pugixml: https://pugixml.org/
- Парсер Xerces-C++: https://xerces.apache.org/xerces-c/
- Специфікація XML від W3C: https://www.w3.org/XML/
