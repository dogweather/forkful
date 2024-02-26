---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:25.495096-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05DE\u05E1\u05E4\u05E7\u05D9\u05DD \u05D3\u05E8\u05DA\
  \ \u05DC\u05D7\u05E4\u05E9, \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D5\u05DC\u05EA\
  \u05E4\u05E2\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05D0\u05DE\
  \u05E6\u05E2\u05D5\u05EA \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05DE\u05D5\u05D2\
  \u05D3\u05E8\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF\
  \ \u05E0\u05E8\u05D7\u05D1 \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\
  \u05D5 \u05D0\u05D9\u05DE\u05D5\u05EA \u05E7\u05DC\u05D8\u05D9\u05DD, \u05E0\u05D9\
  \u05EA\u05D5\u05D7\u2026"
lastmod: '2024-02-25T18:49:38.346319-07:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05DE\u05E1\u05E4\u05E7\u05D9\u05DD \u05D3\u05E8\u05DA\
  \ \u05DC\u05D7\u05E4\u05E9, \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D5\u05DC\u05EA\
  \u05E4\u05E2\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05D0\u05DE\
  \u05E6\u05E2\u05D5\u05EA \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05DE\u05D5\u05D2\
  \u05D3\u05E8\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF\
  \ \u05E0\u05E8\u05D7\u05D1 \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\
  \u05D5 \u05D0\u05D9\u05DE\u05D5\u05EA \u05E7\u05DC\u05D8\u05D9\u05DD, \u05E0\u05D9\
  \u05EA\u05D5\u05D7\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D9\u05DC\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) מספקים דרך לחפש, להתאים ולתפעל מחרוזות באמצעות תבניות מוגדרות. מתכנתים משתמשים בהם באופן נרחב למשימות כמו אימות קלטים, ניתוח נתוני טקסט ומציאת דפוסים בתוך קבצי טקסט גדולים, דבר ההופך אותם לכלי חזק בכל שפה, כולל C.

## איך לעשות:

כדי להשתמש בביטויים רגולריים ב-C, תעבוד בעיקר עם ספריית ה-regex של POSIX (`<regex.h>`). הדוגמה הזו מדגימה התאמת תבנית בסיסית:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // תבנית להתאמת מחרוזות המתחילות ב-'a' אחריו תווים אלפאנומריים
    char *test_string = "apple123";

    // קמפול הביטוי הרגולרי
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Could not compile regex\n");
        exit(1);
    }

    // ביצוע הביטוי הרגולרי
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Match found\n");
    } else if (return_value == REG_NOMATCH) {
        printf("No match found\n");
    } else {
        printf("Regex match failed\n");
        exit(1);
    }

    // שחרור הזיכרון שהוקצה עבור ה-regex
    regfree(&regex);

    return 0;
}
```

פלט לדוגמה עבור מחרוזת התואמת ("apple123"):
```
Match found
```
ולמחרוזת שלא מתאימה ("banana"):
```
No match found
```

## צלילה עמוקה:

ביטויים רגולריים ב-C, כחלק מתקן POSIX, מציעים דרך חזקה לבצע התאמות ותפעול של מחרוזות. עם זאת, ממשק ה-API של ספריית ה-regex של POSIX ב-C נחשב למסורבל יותר מאלו שנמצאו בשפות המעוצבות עם יכולות עיבוד מחרוזות מובנות ברמה גבוהה כמו Python או Perl. התחביר עבור התבניות דומה ברחבי השפות, אך ב-C נדרשת ניהול זיכרון ידני ויותר קוד טקסטואלי להכנה, ביצוע וניקוי לאחר שימוש בתבניות regex.

למרות האתגרים הללו, למידת השימוש ב-regex ב-C מתגמלת מכיוון שהיא מעמיקה את ההבנה במושגים תכנותיים ברמה נמוכה יותר. בנוסף, היא פותחת אפשרויות עבור תכנות ב-C בתחומים כמו עיבוד טקסט וחילוץ נתונים שבהם ה-regex הוא בלתי נפרד. לתבניות מורכבות יותר או לפעולות regex, חלופות כמו ספריית PCRE (Perl Compatible Regular Expressions) עשויות להציע ממשק מועשר יותר בתכונות ומעט יותר פשוט לשימוש, למרות שהן דורשות אינטגרציה של ספרייה חיצונית לפרויקט ה-C שלך.
