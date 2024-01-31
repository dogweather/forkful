---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:38:03.389632-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה המרת מחרוזת לאותיות קטנות ולמה זה נחוץ? זו פעולה שמשנה את כל תווי המחרוזת לאותיות קטנות. תכנתים עושים זאת לעיתים כדי לאחד פורמטים, להקל על השוואת מחרוזות, או להתאים טקסט לסטנדרטים קונקרטיים.

## How to:
מוצגים כאן כמה דוגמאות קוד שמראות איך להמיר מחרוזת לאותיות קטנות ב-Arduino:

```Arduino
String original = "Hello, World!";
String lowerCase = original.toLowerCase();

Serial.begin(9600);
Serial.println(lowerCase); // ידפיס: hello, world!
```

קוד זה ישתמש בפונקציה `.toLowerCase()` כדי להמיר את המחרוזת במשתנה `original` לאותיות קטנות ולאחסן אותה במשתנה `lowerCase`.

## Deep Dive:
ההמרה של מחרוזות לאותיות קטנות היא פרקטיקה קלאסית בתכנות, נפוצה מראשית ימי המחשב האישי. בארדואינו, זה קל במיוחד כי השפה מספקת פונקציית מובנית עבור פעולה זו. אלטרנטיבות כוללות שימוש בלולאות לטיוב כל תו לאות קטנה באופן ידני תוך שימוש בטבלת ASCII. פרטי היישום כוללים התייחסות למקרים שוליים כמו תווים מיוחדים ושפות שונות שמרבה עליהן הפונקציה `.toLowerCase()`.

## See Also:
עיון נוסף בנושאים דומים ניתן למצוא במקורות הבאים:
- [ASCII Table and Description](http://www.asciitable.com/)
- [Arduino String Manipulation](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringCharacters)
