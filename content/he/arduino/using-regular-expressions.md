---
title:                "שימוש בביטויים רגילים"
html_title:           "Fish Shell: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
ביטויים מתוקנים (Regular expressions) הם שפת תכנות מיוחדת שמאפשרת לנו לזהות או לחלץ מדלפים של טקסט ביצועים מסוימים. המתכנתים משתמשים בהם מכיוון שהם משפרים את כלל היעילות והמהירות של הקוד.

## איך לעשות:
```Arduino
#include <regex.h>
regex_t regex;
int Return;

Return = regcomp(&regex, "^a[[:alnum:]]", 0);
if(Return){ /* Handle error */}

Return = regexec(&regex, "subject", 0, NULL, 0);
if(!Return){
  // Match found
}
else if(Return == REG_NOMATCH){
  // No match
}
else {
  // RegExp Error
}
regfree(&regex);
```
בדוגמה זו, אנו מבדיקים אם מחרוזת מתחילה באות 'a' ואז תו אלפא-נומרי.

## כניסה עמקה
ביטויים מתוקנים מוסיפים לא רק יעילות לקוד שלך אלא גם גמישות. הם התפתחו בשנות ה-50 כדי לתמוך בשפת תכנות שנקראת COBOL. חלופות לביטויים מתוקנים כוללות גישות מותאמות אישית שתלויות בפונקציות מחרוזת מתאימות או מנגנונים אחרים שגדלים בפריקות. 

בגרמניקה, כאשר אכן אנו משתמשים בביטויים מתוקנים ב-Arduino, מחלקה שלהם מתאימה למערכת ROm small shell (ROSS). זו הופכת אותם לכלי יעיל למערכות בעלות משאבים מוגבלים.

## ראה גם
1. [מבוא אינטרנטי לביטויים מתוקנים](https://developer.mozilla.org/he/docs/Web/JavaScript/Guide/Regular_Expressions)
2. [מדריך שפת תכנות באינטרנט שבו מוסברות הבניות של ביטויים מתוקנים](https://regexone.com/)
3. [רשימת משאבים ללמידת ביטויים מתוקנים ב-Dev.to](https://dev.to/davidmm1707/regular-expressions-resources-3cf7)