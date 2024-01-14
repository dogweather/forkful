---
title:                "C#: מציאת אורך מחרוזת"
simple_title:         "מציאת אורך מחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה:
המטרה של כתיבת הפוסט הזה היא ללמד קוראים עבריים כיצד להשתמש בשפת תכנות C# כדי למצוא את אורך המחרוזת (string). אנחנו נשתמש בדוגמאות כדי להבין את הנושא הזה בצורה קלה וברורה.

## איך לעשות:
נתחיל עם היצירת משתנה מסוג string ונשמור בתוכו את המחרוזת "שלום". לאחר מכן, נשתמש בפקודת "Length" כדי למצוא את אורך המחרוזת הזו. נדפיס את התוצאה החוזרת על ידי הפקודה באמצעות Console.WriteLine. הנה מספר דוגמאות:
```C#
string str = "שלום";
Console.WriteLine(str.Length);

string message = "היי, איך הייתם?";
Console.WriteLine(message.Length);

string country = "ישראל";
Console.WriteLine(country.Length);
```
הפלט של הקוד הנ"ל יהיה:
```
4
16
5
```

## טיול עמוק:
עכשיו שהבנו איך למצוא את אורך המחרוזת באמצעות הפקודה Length, נתחיל להתייחס לכמה נקודות חשובות יותר. פקודת Length מחזירה את מספר התווים במחרוזת, כולל רווחים וסימני פיסוק. בנוסף, זה יעבוד גם עבור מחרוזות שמכילות תווים באורך שונה, כך שאין צורך להיות מודעים מראש לכמה תווים יש במחרוזת כדי להשתמש בפקודה זו.

## ראו גם:
* [דוגמאות נוספות עבור פקודת "Length"](https://www.geeksforgeeks.org/c-sharp-string-length-property/)

* [מדריך למציאת האורך של מחרוזת בשפת C#](https://www.programiz.com/csharp-programming/numbers-strings)

* [מאמר על פקודת "Length" בכתב השורות של ישראל הצעיר](https://www.youngisrael-stl.org/)