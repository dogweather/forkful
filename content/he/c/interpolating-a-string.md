---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

אינטרפולציה של מחרוזת היא החלפת משתנים או ערכים במחרוזת באופן דינאמי. מתכנתים משתמשים בכך כדי ליצור מחרוזות שאינן קבועות, ולשנות אותן בהתאם למידע שהם מקבלים בזמן ריצה.

## איך?
```C
#include <stdio.h>

int main()
{
    char name[] = "Yosef";
    printf("Shalom, %s!\n", name); 
    return 0;
}
```
פלט דוגמה:
```
Shalom, Yosef!
```

## יותר לעומק
1. אינטרפולציה של מחרוזת הגיעה לאחרונה לשפת C, אך היא מהותית בשפות תכנות אחרות כמו ג'אווהסקריפט ופייתון.
2. כשאתם צריכים להדפיס מחרוזת מורכבת, אתם יכולים להשתמש ב- sprintf - זו שיטה חלופית אך זהירים! זו תהיה רגישה להפצצת מטמון.
3. ב-C, הפונקציה printf משנה את המחרוזת. כאשר אנחנו מניחים %s בתוך המחרוזות, רק אז, בזמן ריצה, printf תחליף את %s עם הערך של המשתנה.

## לעיון נוסף
1. הסבר על אינטרפולציה של מחרוזת ב- Java: `https://www.vogella.com/tutorials/JavaIntroduction/article.html#stringinterpolation`
2. הסבר על אינטרפולציה של מחרוזת ב- JavaScript: `https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#string_operators`