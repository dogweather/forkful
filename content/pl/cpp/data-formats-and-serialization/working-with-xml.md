---
date: 2024-01-26 04:28:40.573333-07:00
description: "Praca z XML oznacza parsowanie, tworzenie i manipulowanie danymi XML\
  \ (eXtensible Markup Language). Programi\u015Bci zarz\u0105dzaj\u0105 XML, aby obs\u0142\
  ugiwa\u0107 wymian\u0119\u2026"
lastmod: '2024-03-13T22:44:35.737122-06:00'
model: gpt-4-0125-preview
summary: "Praca z XML oznacza parsowanie, tworzenie i manipulowanie danymi XML (eXtensible\
  \ Markup Language). Programi\u015Bci zarz\u0105dzaj\u0105 XML, aby obs\u0142ugiwa\u0107\
  \ wymian\u0119\u2026"
title: Praca z XML
weight: 40
---

## Co i dlaczego?
Praca z XML oznacza parsowanie, tworzenie i manipulowanie danymi XML (eXtensible Markup Language). Programiści zarządzają XML, aby obsługiwać wymianę danych strukturalnych, konfigurację i więcej, ze względu na jego neutralność platformową.

## Jak to zrobić:
Oto prosty sposób na parsowanie XML za pomocą biblioteki TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Witaj, świecie!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

Przykładowe wyjście:

```
Witaj, świecie!
```

A tak tworzy się plik XML:

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
    message->SetText("Witaj, świecie!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

To generuje plik XML `output.xml` z zawartością:

```xml
<?xml version="1.0"?>
<root>
    <message>Witaj, świecie!</message>
</root>
```

## Pogłębiona analiza
XML był kluczowy w usługach internetowych i przechowywaniu danych od końca lat '90. Chociaż obecnie częściej używa się JSON i YAML do konfiguracji i interop, XML nadal ma ogromne znaczenie w wielu systemach przedsiębiorstw. Parsowanie XML w C++ może wydawać się staromodne z ręcznym parsowaniem DOM/SAX. Na szczęście, biblioteki takie jak TinyXML-2 to upraszczają. C++ nie ma wbudowanego wsparcia dla XML; biblioteki takie jak TinyXML-2, pugixml czy Xerces ułatwiają trudne zadania.

## Zobacz również
- Dokumentacja TinyXML-2: https://leethomason.github.io/tinyxml2/
- Biblioteka pugixml: https://pugixml.org/
- Parser Xerces-C++: https://xerces.apache.org/xerces-c/
- Specyfikacja XML W3C: https://www.w3.org/XML/
