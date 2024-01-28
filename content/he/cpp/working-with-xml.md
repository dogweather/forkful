---
title:                "עבודה עם XML"
date:                  2024-01-26T04:28:48.882963-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML פירושה ניתוח, יצירה, ומניפולציה של נתוני XML (eXtensible Markup Language - שפת סימון מתקדמת). מתכנתים מנהלים XML כדי לטפל בהחלפת נתונים מובנים, תצורה, ועוד, בשל אופיים הניטרלי מבחינה פלטפורמלית.

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
