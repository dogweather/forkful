---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:54.895667-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1-C \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7, \u05E9\u05D0\u05D9\u05DC\u05EA\
  \u05D0, \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05DE\u05E1\u05DE\u05DB\u05D9 XML\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA\
  \ \u05E9\u05D5\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05EA\
  \u05E2\u05E1\u05E7\u05D9\u05DD \u05E2\u05DD XML \u05D1\u05E9\u05DC \u05E9\u05D9\u05DE\
  \u05D5\u05E9\u05D5 \u05D4\u05E0\u05E8\u05D7\u05D1 \u05D1\u05E9\u05D9\u05E8\u05D5\
  \u05EA\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05E7\u05D1\u05E6\u05D9\
  \ \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA, \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\u2026"
lastmod: 2024-02-19 22:04:59.448793
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1-C \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7, \u05E9\u05D0\u05D9\u05DC\u05EA\u05D0\
  , \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05DE\u05E1\u05DE\u05DB\u05D9 XML \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E9\
  \u05D5\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05EA\u05E2\
  \u05E1\u05E7\u05D9\u05DD \u05E2\u05DD XML \u05D1\u05E9\u05DC \u05E9\u05D9\u05DE\u05D5\
  \u05E9\u05D5 \u05D4\u05E0\u05E8\u05D7\u05D1 \u05D1\u05E9\u05D9\u05E8\u05D5\u05EA\
  \u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05E7\u05D1\u05E6\u05D9 \u05D4\
  \u05D2\u05D3\u05E8\u05D5\u05EA, \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם XML ב-C כוללת ניתוח, שאילתא, ושינוי מסמכי XML באמצעות ספריות שונות. תכנתים מתעסקים עם XML בשל שימושו הנרחב בשירותי אינטרנט, קבצי הגדרות, והחלפת נתונים בין מערכות שונות, מה שדורש כישורים בטיפול יעיל ב-XML עבור פיתוח אפליקציות עמידות.

## איך לעשות:

C אינו תומך כברירת מחדל ב-XML,  לכן יהיה עליכם להשתמש בספריות חיצוניות. אחת הבחירות הפופולריות היא `libxml2` , ספרייה יציבה ועשירת תכונות. הנה כיצד לקרוא ולנתח קובץ XML באמצעות `libxml2`.

ראשית, ודאו ש-`libxml2` מותקן במערכת שלכם. ייתכן שתצטרכו להתקינו דרך מנהל החבילות שלכם (למשל, `apt-get install libxml2-dev` במערכות Debian).

לאחר מכן, כלולו את כותרת `libxml2` בתוכנית C שלכם:

```c
#include <libxml/parser.h>
#include <libxml/tree.h>
```

כעת, נכתוב תוכנית פשוטה לניתוח קובץ XML ולהדפסת שמות האלמנטים ברמה הראשונה:

```c
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main(void) {
    xmlDoc *document = NULL;
    xmlNode *root_element = NULL;

    // אתחול הספרייה ובדיקת חוסר תאימות ABI
    LIBXML_TEST_VERSION

    // נתח את הקובץ וקבל את ה-DOM
    document = xmlReadFile("your_file.xml", NULL, 0);

    if (document == NULL) {
        printf("Failed to parse the XML file\n");
        return -1;
    }

    // קבל את צומת אלמנט השורש
    root_element = xmlDocGetRootElement(document);

    for (xmlNode *currentNode = root_element; currentNode; currentNode = currentNode->next) {
        if (currentNode->type == XML_ELEMENT_NODE) {
            printf("Node Type: Element, name: %s\n", currentNode->name);
        }
    }

    // שחרור הזיכרון שהוקצה עבור המנתח וה-DOM
    xmlFreeDoc(document);

    // ניקוי ובדיקת דליפות
    xmlCleanupParser();
    xmlMemoryDump(); // אופציונלי

    return 0;
}
```

לקמפול התוכנית הזו, ודאו שאתם מקשרים אותה נגד `libxml2`:

```sh
gcc -o xml_example xml_example.c $(xml2-config --cflags --libs)
```

בהנחה שיש לכם קובץ XML בשם `your_file.xml`, הרצת התוכנית המקומפלת אמורה להדפיס את שמות האלמנטים ברמה הראשונה שלה.

## צלילה עמוקה

האינטראקציה בין C ל-XML היא סיפור של הבאת שתי עולמות שונים לחלוטין יחד: הפרדיגמה המבנית, ברמת הבית, הפרוצדורלית של C והמודל ההיררכי, המילולי והממוקד מסמכים של XML. כאשר משלבים יכולות טיפול ב-XML בתוכניות C, מפתחים מנצלים את חוזקות C - כגון מהירות וגישה לזיכרון ברמה נמוכה - כדי לנתח ולשנות מסמכי XML ביעילות.

`libxml2`, שפותח כחלק מפרויקט GNOME, התפתח לתקן המקובל לעיבוד XML ב-C בזכות תמיכתו המקיפה בתקני XML וביצועיו. הוא מגולם שנים של מאמץ פיתוח ותרומות קהילתיות, והופך אותו ליעיל וחזק עבור רוב משימות ה-XML.

למרות ש-`libxml2` מציע יכולות חזקות, ראוי לציין כי סיבוך של ניתוח ושינוי XML יכול להכניס עומס ניכר. בתרחישים בהם המילוליות והמורכבות של XML אינם מוצדקים, חלופות כמו JSON עשויות להיות מועדפות להחלפת נתונים. עם זאת, עבור יישומים או סביבות בהן השימוש ב-XML מושרש, אימון בשימוש ב-`libxml2` ב-C מאפשר התמודדות עם מגוון רחב של מסמכים ו-APIs של XML, ומגשר על הפער בין שפת התכנות C לעולם עיבוד המסמכים המבניים.
