---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Arduino: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרה של מחרוזת לאותיות קטנות היא תהליך שבו נעביר את כל האותיות במחרוזת למצב של אותיות קטנות בלבד. תהליך זה נעשה על מנת להקל על השוואת מחרוזות ועיבודן, וכן להיות אחידים בכתיבת קוד.

## איך לעשות זאת:
קובץ קוד הבא מראה דוגמאות של כיצד לממש את המרה של מחרוזת לאותיות קטנות בשפת ארדוינו:
```arduino
String str = "Hello World!";
Serial.println(str.toLowerCase()); // פלט: hello world!
```

## כיול עמוק:
מרבית תכניות התכנות מכילות פונקציה מובנית להמרת מחרוזת לאותיות קטנות, כגון בשפת C++ שמקורה של שפת ארדוינו. עם זאת, דוגמאות של תוכניות מתכנתים צריכות להיות וריאציות של הקוד הנתון בכדי לשמש את המטרה האמורה.

## ראה גם:
- [מדריך לשימוש בפונקציות מחרוזות בארדוינו](https://www.arduinolibraries.info/libraries/string)
- [הספריה האופנה למיקרו קונטרולרים](https://www.arduino.cc/en/Reference/StringObject)