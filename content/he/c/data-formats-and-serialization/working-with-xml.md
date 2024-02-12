---
title:                "עבודה עם XML"
aliases:
- /he/c/working-with-xml/
date:                  2024-02-03T18:13:54.895667-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-xml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
