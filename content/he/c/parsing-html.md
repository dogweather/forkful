---
title:                "פרשים את ה־HTML"
html_title:           "C: פרשים את ה־HTML"
simple_title:         "פרשים את ה־HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
הִיפֶּרְטִינְג (parsing) של ה-HTML הוא תהליך שבו מתבצעת קריאה וניתוח של קוד ה-HTML בכדי להבין וליישם את המידע המכיל בו. תהליך זה חשוב מאוד למתכנתים שמעוניינים לעבוד עם אתרי אינטרנט או לכתוב כלי עזר כגון ניתוחי אתרים אוטומטיים.

## איך לעשות:
```
// כאן מופיע קוד בשפת C 
// לדוגמה, פונקציה שלדאגת קריאה ופרסור של דף HTML באמצעות ספריית libxml2
#include <stdio.h>
#include <libxml/parser.h>

int main()
{
  // הגדרת משתנים רלוונטים
  xmlDoc *doc = NULL;
  xmlNode *root_element = NULL;

  // קריאת דף HTML שבו נרצה לבצע פרסור
  doc = xmlReadFile("example.html", NULL, 0);

  // בדיקה שהקריאה התבצעה בהצלחה
  if (doc == NULL)
  {
    printf("קריאת הדף נכשלה.");
    return 1;
  }

  // צימוד לאירועים של הדף ובחירת האלמנט הראשון
  root_element = xmlDocGetRootElement(doc);

  // הדפסת תוצאות הקריאה והפרסור של הדף HTML
  printf("שם האלמנט הראשי: %s\n", root_element->name);

  // השתחררות מזיכרון פנימי
  xmlFreeDoc(doc);

  return 0;
}
```

## נכיר בעומק:
תהליך הפרסור של HTML קיים כבר מאז זמן רב והתחיל במטרה להפיק מידע מדויק מקוד ה-HTML בכדי לאפשר לכלי חיפוש כמו גוגל לבנות את המפתחות לניווט באינטרנט. כיום, ישנן אפשרויות רבות לפרסור HTML כגון באמצעות כלי חיצוני כמו פייתון או JavaScript ולא רק עם שפת תכנות גנרית כמו C.

## ראו גם:
למידה נוספת על הפעולות של פרסור HTML בשפת C והאפשרויות השונות שלו ניתן למצוא בכתב אחר: https://www.geeksforgeeks.org/web-scrapping-c-programming-language/