---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
משנה תת-מחרוזות הוא סוג של פעולה שבהאנו מחלצים מחרוזת משנה מחרוזת ראשית. זה נמשך בשל הצורך לנתח דינמיקלית מחרוזות מקור, לבדוק קטעים מסוימים, או לבצע את האם בשעה שאנחנו כותבים את הקוד.

## איך לבצע:
הנה דוגמא של קוד בו אנחנו משנים תת-מחרוזת מהמחרוזת הראשית בשפת C:

```C
#include <stdio.h>
#include <string.h>

void main() {
    char mainString[30] = "Coding in Hebrew is awesome";
    char subString[15];

    strncpy(subString, mainString + 11, 6);  // Extract "Hebrew"
    subString[6] = '\0';

    printf("%s\n", subString);  // Print extracted sub string
}
```

אם נפעיל דגם של הקוד הזה הפלט יהיה:

```
Hebrew
```

## צפיה מרובה:
לאורך השנים, שפות תכנות שונות נתנו אפשרויות שונות למימוש למניעת משנה מחרוזות. בשפת C, פונקציה `strncpy()` היא אחת מהדרכים הנפוצות ביותר לבצע זאת. אך יש לזכור שישנן שיטות נוספות כמו `sscanf()`, `strchr()` ואחרות.
השימוש במשני מחרוזות יכול להימשך באופן רחב בתכנות, למשל בפענוח מחרוזות, במערכות בהן מחרוזות מפנה נשלחות ונקלטות, או בסטרינג המורכב מחלקים הנושאים משמעויות שונות.

## ראה גם:
[מניעת המשנה מחרוזת ב- C](https://he.code-examples.net/q/Q6A9)
[הפונקציה `strncpy`](https://manual.he.net/c/library/string.h/strncpy)
[בדיקת שימוש במשנה מחרוזת](https://stackoverflow.com/questions/4214314/get-a-substring-of-a-char)