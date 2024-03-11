---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:11.540117-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D1-C \u05DB\u05D5\u05DC\u05DC\u05D9\u05DD \u05D0\u05D9\
  \u05EA\u05D5\u05E8 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DE\
  \u05E1\u05D5\u05D9\u05DE\u05D5\u05EA \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05D5\
  \u05D4\u05D7\u05DC\u05E4\u05EA\u05DF \u05D1\u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\
  \u05EA \u05D0\u05DC\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E0\u05D9\u05E4\u05D5\
  \u05DC \u05E0\u05EA\u05D5\u05E0\u05D9 \u05D8\u05E7\u05E1\u05D8 -\u2026"
lastmod: '2024-03-11T00:14:13.600318-06:00'
model: gpt-4-0125-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D1-C \u05DB\u05D5\u05DC\u05DC\u05D9\u05DD \u05D0\u05D9\u05EA\
  \u05D5\u05E8 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DE\u05E1\
  \u05D5\u05D9\u05DE\u05D5\u05EA \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05D5\u05D4\
  \u05D7\u05DC\u05E4\u05EA\u05DF \u05D1\u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\
  \ \u05D0\u05DC\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9 \u05D8\u05E7\u05E1\u05D8 -\u2026"
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05D4 \u05E9\
  \u05DC \u05D8\u05E7\u05E1\u05D8"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט ב-C כוללים איתור תת-מחרוזות מסוימות בתוך מחרוזת גדולה יותר והחלפתן בתת-מחרוזות אחרות. מתכנתים מבצעים פעולות אלה כדי למניפול נתוני טקסט - למשימות החל מניקוי ועיצוב נתונים ועד יצירה דינמית של תוכן.

## איך לעשות:

C אינה מגיעה עם פונקציות מובנות לביצוע חיפוש והחלפה ישירות על מחרוזות. עם זאת, ניתן להשיג זאת על ידי שילוב של פונקציות שונות לטיפול במחרוזות שזמינות בספריית ה-`<string.h>` יחד עם היגיון מותאם אישית. להלן דוגמה בסיסית לכיצד לחפש תת-מחרוזת בתוך מחרוזת ולהחליפה. לפשטות, הדוגמה הזו מניחה גודל באפר מספיק ואינה מטפלת בבעיות הקצאת זיכרון שיש לשקול בקוד ייצור.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // חישוב אורך עד להתאמה
        len_up_to_match = tmp - source;
        
        // העתקת החלק לפני ההתאמה
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // העתקת התת-מחרוזת החדשה
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // התקדמות מעבר להתאמה במחרוזת המקור
        tmp += len_sub;
        source = tmp;
    }
    
    // העתקת כל חלק נותר של המחרוזת המקורית
    strcpy(insert_point, source);
    
    // הדפסת המחרוזת המעודכנת
    printf("Modified string: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Hello, this is a test. This test is simple.";
    char sub[] = "test";
    char newSub[] = "sample";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

פלט לדוגמה:
```
Modified string: Hello, this is a sample. This sample is simple.
```

הקוד הזה מדגים גישה פשוטה לחיפוש כל מופעי תת-מחרוזת (`sub`) בתוך מחרוזת מקור ולהחליפם בתת-מחרוזת אחרת (`newSub`), באמצעות הפונקציה `strstr` למציאת נקודת ההתחלה של כל התאמה. זוהי דוגמה יסודית מאוד שאינה מטפלת בתרחישים מורכבים כמו תת-מחרוזות מתקבלות.

## עיון נוסף

הגישה שנעשתה בסעיף "איך לעשות" היא יסודית, הממחישה כיצד לבצע חיפוש והחלפה של טקסט ב-C ללא ספריות צד שלישי. היסטורית, בשל דגשה של C על ניהול זיכרון ברמה הנמוכה וביצועים, ספריית התקן שלה לא מכילה פונקציונליות מתקדמת למניפולציה על מחרוזות כמו אלו הנמצאות בשפות כמו Python או JavaScript. מתכנתים חייבים לנהל זיכרון באופן ידני ולשלב פעולות מחרוזות שונות כדי להשיג את התוצאות הרצויות, מה שמגביר את המורכבות אך מציע יותר שליטה ויעילות.

חשוב לציין שגישה ידנית זו עלולה להיות נוטה לשגיאות, במיוחד בניהול הקצאות זיכרון וגדלי באפרים. טיפול לא נכון יכול להוביל לשפיכות באפר והשחתת זיכרון, מה שהופך את הקוד לפגיע לסיכוני אבטחה.

במקרים רבים מעשיים, במיוחד אלו הדורשים עיבוד טקסט מורכב, לעיתים כדאי לשקול אינטגרציה של ספריות צד שלישי כמו PCRE (Perl Compatible Regular Expressions) עבור חיפוש והחלפה מבוססי regex, אשר יכולים לפשט את הקוד ולהפחית את הסיכוי לשגיאות. בנוסף, תקנים ומהדרים מודרניים של C מציעים פונקציות מובנות וחלופות בטוחות יותר למניפולציה של מחרוזות, במטרה להקטין את הפקקות הנפוצות הנצפות במאגרי קוד C ישנים יותר. עם זאת, הבנה יסודית של עיבוד טקסט ידני נותרת כישור יקר ערך בארגז הכלים של מתכנת, במיוחד ליישומים ביצועים-קריטיים.
