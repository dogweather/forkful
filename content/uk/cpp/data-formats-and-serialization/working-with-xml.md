---
date: 2024-01-26 04:28:41.213560-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u0440\u043E\u0437\u0431\u0456\u0440, \u0441\u0442\u0432\u043E\
  \u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u043C\u0430\u043D\u0456\u043F\u0443\
  \u043B\u044F\u0446\u0456\u0457 \u0437 \u0434\u0430\u043D\u0438\u043C\u0438 XML (\u0440\
  \u043E\u0437\u0448\u0438\u0440\u044E\u0432\u0430\u043D\u0430 \u043C\u043E\u0432\u0430\
  \ \u0440\u043E\u0437\u043C\u0456\u0442\u043A\u0438). \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u043A\u0435\u0440\u0443\u044E\u0442\u044C\
  \ XML \u0434\u043B\u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0441\u0442\
  \u0440\u0443\u043A\u0442\u0443\u0440\u043E\u0432\u0430\u043D\u043E\u0433\u043E \u043E\
  \u0431\u043C\u0456\u043D\u0443\u2026"
lastmod: '2024-03-13T22:44:49.889138-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u0440\u043E\u0437\u0431\u0456\u0440, \u0441\u0442\u0432\u043E\
  \u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u043C\u0430\u043D\u0456\u043F\u0443\
  \u043B\u044F\u0446\u0456\u0457 \u0437 \u0434\u0430\u043D\u0438\u043C\u0438 XML (\u0440\
  \u043E\u0437\u0448\u0438\u0440\u044E\u0432\u0430\u043D\u0430 \u043C\u043E\u0432\u0430\
  \ \u0440\u043E\u0437\u043C\u0456\u0442\u043A\u0438)."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
