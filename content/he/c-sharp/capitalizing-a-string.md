---
title:    "C#: הפיכת מחרוזת לאותיות גדולות"
keywords: ["C#"]
---

{{< edit_this_page >}}

## למה

כתיבת קוד מאתגרת ומעניינת תמיד דורשת מגוון כלים וטכניקות. אחת מן הטכניקות שיכולות להוסיף עניין ושיפור לקוד היא לכתוב פקודה להפיכה של תווים במחרוזת לאותיות גדולות. במאמר זה נכיר כיצד לעשות זאת בעזרת שפת התכנות C#.

## איך לעשות

כדי להפוך את כל האותיות במחרוזת לאותיות גדולות, אנו נשתמש בפקודת ToUpper בשפת C#. נתחיל ביצירת מחרוזת פשוטה שאנו רוצים להפוך את אותיותיה לאותיות גדולות.

```C#
string str = "hello world";
string capitalizedStr = str.ToUpper();  

Console.WriteLine(capitalizedStr);
```

בקוד זה, אנו משתמשים במשתנה שמכיל את המחרוזת המקורית ומשתנה חדש שנכיל את המחרוזת המכילה את האותיות הגדולות. נעבור לשלוח כתובת המשתמש על מנת לתת לו הברירה להזין את המחרוזת שהוא ירצה להפוך לאותיות גדולות.

```C#
string str = Console.ReadLine();
string capitalizedStr = str.ToUpper();

Console.WriteLine(capitalizedStr);
```

רוצים להתחייב לכתוב תוכניות קוד חכמות יותר? אפשר להשתמש בפקודת ToLower כדי להמיר את האותיות לאותיות קטנות. כך, יתאפשר ליצור את השורות הבאות.

```C#
string str = Console.ReadLine();
if (str == "hello") {
    Console.WriteLine("Hello there!");
} else {
    Console.WriteLine("Sorry, I did not recognize that input.");
}

string lowerCaseStr = str.ToLower();

if (lowerCaseStr == "hello") {
    Console.WriteLine("Hello there!");
} else {
    Console.WriteLine("Sorry, I still did not recognize that input.");
}
```

בפקודה הראשונה, אנו בודקים אם הטקסט הנכנס הוא "hello" באמצעות פעולת השוואה ישירה. אחר כך, אם הטקסט זהה בדיוק, נדפיס את המלים "Hello there!". בפקודה השנייה, אנו משתמשים בפקודה ToLower כדי להמיר את הטקסט לאותיות