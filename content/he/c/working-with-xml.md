---
title:                "עבודה עם XML"
date:                  2024-01-26T04:28:51.789142-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML ב-C כוללת ניתוח, יצירה, ושינוי של קבצי XML - במהותה, אחסון נתונים מבוסס מבנה. מתכנתים עושים זאת על מנת להתעסק עם נתונים בפורמט נייד וקריא לאדם, שנפוץ לשימוש בקונפיגורציה, החלפת נתונים ועוד.

## איך ל:
למטה נמצא קטע קוד המשתמש בספריית `libxml2` לניתוח קובץ XML ולקיחת האלמנט השורש.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // ניתוח הקובץ XML
    doc = xmlReadFile("example.xml", NULL, 0);

    // קבלת האלמנט השורש
    root_element = xmlDocGetRootElement(doc);

    printf("Root Element: %s\n", root_element->name);

    // שחרור המסמך
    xmlFreeDoc(doc);

    // ניקוי המפענח
    xmlCleanupParser();

    return 0;
}
```

דוגמה לפלט עבור XML עם שורש `<data>` עשויה להיות:
```
Root Element: data
```

## צלילה לעומק
XML, או תיאור שפה מסמנת המתפתחת, חוזרת לסוף שנות ה-90, מספקת דרך לתאר ולמבנת נתונים. ב-C, `libxml2` היא האפשרות המועדפת. היא חזקה, אם כי לא הכי קלה למתחילים בעבודה עם XML. אלטרנטיבות כוללות את `tinyxml2`, שהיא קלילה ויותר ידידותית למתחילים. לגבי היישום, ב-C אין תמיכה מובנית ב-XML, כך שספריות ממלאות את הפער. הן משתנות בגודלן, מהירותן, מורכבותן, וניידותן. רובן מציעות שיטות ניתוח DOM ו-SAX: DOM טוען את הכל לזיכרון, טוב למסמכים קטנים; SAX מבוססת אירועים, מטפלת באלמנטים כך שהם מופיעים, טובה יותר לקבצים גדולים. לכל אחת מהן יש מקרי שימוש והסכמים של יתרונות וחסרונות.

## ראה גם
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 ב-GitHub](https://github.com/leethomason/tinyxml2)
- [מדריך XML באתר w3schools](https://www.w3schools.com/xml/)
- [מפרט XML של W3C](https://www.w3.org/XML/)
