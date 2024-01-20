---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת מחרוזת לאותיות קטנות היא תהליך שמשנה את כל האותיות הגדולות במחרוזת לאותיות קטנות. מתכנתים משתמשים בכך כדי לקבל נתונים עקביים לשם בדיקה או מניפולציה.

## איך לעשות זאת:
```Arduino
String MyString = "HELLO, WORLD!";
MyString.toLowerCase();
Serial.begin(9600);
Serial.println(MyString);
```
הפלט המצופה הוא: "hello, world!"

## עיון מעמיק:
בשנים הראשונות של מדעי המחשב, לא הייתה הפרדה בין אותיות גדולות לאותיות קטנות, מה שהיווה בעיה בהקלדת שמות משתנים או מחרוזות טקסט. מאז, חלו הרבה שינויים. היום, מקודדים מרובים כמו UTF-8 מאפשרים המרה לאותיות קטנות בצורה יעילה ומהירה.

אם יש צורך לביצוע המרת אותיות בעצמך, אפשר להשתמש בפונקציה `tolower`. ראה לדוגמה הבאה:
```Arduino
char c = 'H';
c = tolower(c);
Serial.begin(9600);
Serial.println(c);
```
הפלט המצופה הוא: "h"

## ראו גם:
1. [תיעוד ה-API של Arduino עבור סטרינג](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. [דוגמאת קוד של תיעוד Arduino להמרת מחרוזת לאותיות קטנות](https://www.arduino.cc/en/Tutorial.BuiltInExamples.StringCaseChanges)
3. [תיעוד C++ עבור 'tolower'](https://en.cppreference.com/w/cpp/string/byte/tolower)