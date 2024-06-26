---
date: 2024-01-26 04:28:48.882963-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E0\u05D4\
  \ \u05D3\u05E8\u05DA \u05E4\u05E9\u05D5\u05D8\u05D4 \u05DC\u05E0\u05EA\u05D7 XML\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 TinyXML-2."
lastmod: '2024-03-13T22:44:39.876479-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E0\u05D4 \u05D3\u05E8\u05DA \u05E4\u05E9\u05D5\u05D8\u05D4 \u05DC\
  \u05E0\u05EA\u05D7 XML \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 TinyXML-2."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך לעשות:
הנה דרך פשוטה לנתח XML באמצעות הספרייה TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>שלום עולם!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

פלט לדוגמא:

```
שלום עולם!
```

וכך יוצרים קובץ XML:

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
    message->SetText("שלום עולם!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

דבר זה יוצר קובץ XML `output.xml` עם התוכן:

```xml
<?xml version="1.0"?>
<root>
    <message>שלום עולם!</message>
</root>
```

## צלילה לעומק
XML היה משמעותי בשירותי אינטרנט ואחסון נתונים מאז שנות ה-90 המאוחרות. למרות שכיום JSON ו-YAML נפוצים יותר לצורכי תצורה ואינטרופרביליות, XML עודנו משמעותי מאוד במערכות ארגוניות רבות. ניתוח XML ב-C++ יכול להרגיש קצת ישן עם ניתוח DOM/SAX ידני. לשמחתנו, ספריות כמו TinyXML-2 מפשטות את התהליך. ל-C++ אין תמיכה מובנית ב-XML; ספריות כמו TinyXML-2, pugixml, או Xerces מספקות פתרונות לאתגרים אלו.

## ראו גם
- תיעוד TinyXML-2: https://leethomason.github.io/tinyxml2/
- ספריית pugixml: https://pugixml.org/
- Xerces-C++ Parser: https://xerces.apache.org/xerces-c/
- מפרט XML של W3C: https://www.w3.org/XML/
