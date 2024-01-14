---
title:                "Arduino: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה:

מחלקת הטכנולוגיות גודלת תמיד במהירות ואנשים רבים מתחילים ללמוד איך לתכנת בשפת ארדוינו. אחת הכישרונות החשובים שאדם יכול ללמוד הוא איך מחליף טקסט בשפת תכנות זו. במאמר זה ניתן תיכנן כיצד לגשת לחיפוש והחלפת טקסט בשפת ארדוינו.

## איך לעשות:

הנה דוגמא לקוד שאנחנו נערוך כדי לעשות חיפוש והחלפת טקסט על סטרימטפס דרך ממשק מחשב:

```Arduino 
while(serial.available()){
  String input = serial.readString();
  input.replace("Hello", "שלום");
  Serial.println(input); 
}
```

כאן אנחנו משתמשים בפונקציית replace שמחליפה את המילה "Hello" עם המילה "שלום". את הטקסט שנדרש לחיפוש והחלפה ניתן לשנות לפי הצורך. במקרה שלנו, אנחנו מדפיסים את הטקסט המעודכן על ידי השתמשות בפונקציית println.

## העמקה:

כדי להבין טוב יותר כיצד להחליף טקסט בשפת ארדוינו, יש להבין שישנן פונקציות טקסט מובנות בשפה זו. אחת הפונקציות החשובות הינה replace המאפשרת לחלוף מחרוזת אחת לאחרת. ישנן גם פונקציות נוספות כמו substring שמאפשרת לקבל חלק מהמחרוזת המקורית. כדי להשתמש בפונקציות אלו ניתן להוסיף את הספרייה String המגדירה את כל הפונקציות הנוספות לטקסט.

## ראה גם:

- [Documentation for Arduino String library](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Tutorial on using String functions in Arduino](https://www.tutorialspoint.com/arduino/arduino_strings.htm)
- [Guide to using search and replace in C++](https://www.cplusplus.com/reference/string/string/replace/)