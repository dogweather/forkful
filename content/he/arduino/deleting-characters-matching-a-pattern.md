---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים התואמים לדפוס היא פעולה חוזרת ונשנית בתכנות, המאפשרת לנו ל"Mאתחל" גורם מסוים למצבו הראשוני. תהליך זה משמש להפך גזרה, מנקה את הרעש או מתקן מידע פגום.

## איך לעשות:

כאן ניקח את `str` ונסיר ממנו את כל התווים המתאימים לדפוס.

```Arduino
String str = " אני מתכנת Arduino מיומנות";
str.replace("Arduino", "");
Serial.println(str);
```

הפלט יהיה: "אני מתכנת מיומנות"

## צלילה עמוקה

השימוש במחיקת תווים המתאימים לדפוס הוא מעין שיטת הגישה לתכנות מהימים הראשונים של המחשבים עצמם. שמקורו בתרבית התכנות בה המפתחים התמודדו עם משאבים מוגבלים. כגון מקום זיכרון.

לחלופין, אם יש לנו את  `regex.h` (bibliotheca de defined regular expressions), נוכל להשתמש בה כדי למחוק תווים המתאימים לדפוס בצורה מסודרת ועקבית.

אמנם Arduino עשוי להתמודד עם הגבלות מקום, אך לא לשם זה לא מאוד מאמצים את regex. במקום זה, אנו משתמשים בעזרה כמו "String"-.replace()

## ראה גם

* [Arduino Reference - String replace()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
* [C++ Reference - std::regex](http://www.cplusplus.com/reference/regex/)
* [Arduino Forum - Delete characters from string](https://forum.arduino.cc/index.php?topic=486320.0)