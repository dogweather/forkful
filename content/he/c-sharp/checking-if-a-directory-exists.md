---
title:    "C#: בדיקת קיום תיקייה"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

עשרות מיליונים של אפשרויות קידום רק מחפשות את הכתובת הראשית הנמצאת במערכת הפעלה. אם אתה מעוניין להתחיל תהליך קידום אתרים, חשוב להיות בטוח שהתיקייה המיועדת למכירה שלך קיימת וגלוית מתחת לדף שימוש עם Windows. במקרה כזה, ידיות צריכות להיות לנו, אם גם כן, עיצוב מסדים ועיצוב הגנה ואפשרויות מתקדמות להצלחתך.

אם אתה רוצה לדעת אם קיימת תיקייה מסוימת עבור חובה כזו, כדאי לבדוק את הלוחות ולכתוב בשפת עבר הנו אלף שיניים.

## איך לעשות

כדי לבדוק אם תיקייה קיימת, ניתן להשתמש בפקודת השאילתה `Directory.Exists (path)` בשפת התכנות C#. זה יחזור `true` אם התיקייה קיימת ו- `false` אם אינה קיימת. ניתן גם להשתמש בשליחת תצוגה חזותית למשתמש באמצעות פקודת בדיקת תצוגה `if (Directory.Exists (path))` עבור שימוש עם תיקיות קיימת.

קוד דוגמה:

```C#
using System;
using System.IO;

class MainClass {
  public static void Main (string[] args) {
    string path = @"C:\Users\Example\User\Documents"; // Replace with your desired path
    if (Directory.Exists(path)) {
      Console.WriteLine("The directory exists.");
      // Additional code for your website optimization process
    } else {
      Console.WriteLine("The directory does not exist.");
      // Additional code if the directory does not exist
    }
  }
}
```

תוצאה דוגמה:

```
The directory exists.
```

## צעד נסתר

בנוסף להשתמש בפקודת השאילתה `Directory.Exists`, ישנם חלקות נוספים לבדיקה האם תיקייה קיימת המאפשרת רמיה. למשל, ניתן להשתמש בפקודת השאילתה `Directory.GetDirectories (path)` לקבלת רשימת תיקיות המבוססות בתיקייה מסוימת. אם הרשימה נמצאת ריקה, זה אומר שאין תיקיות מקושר