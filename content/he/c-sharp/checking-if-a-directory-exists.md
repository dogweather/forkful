---
title:                "לבדיקת קיום תיקייה במחשב"
html_title:           "C#: לבדיקת קיום תיקייה במחשב"
simple_title:         "לבדיקת קיום תיקייה במחשב"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## עבור למה
כאשר אנו בוחנים אם תיקייה קיימת במחשב שלנו, אנו בעצם בודקים האם ישנו מקום זמין לאחסון קבצים ותיקיות חדשות. זה יכול להיות שימושי לצורך יצירת קבצים או יישומי ניהול תיקיות.

## כיצד לבדוק האם תיקייה קיימת
```C#
if(Directory.Exists("C:\\MyFolder")) 
{
    Console.WriteLine("התיקייה קיימת!"); 
} 
else 
{ 
    Console.WriteLine("התיקייה אינה קיימת!"); 
}
```

פתרון פשוט ויעיל המשתמש בשיטת `Exists()` המגיעה מספריית הקלטות היבשות של שפת סי-שארפ, מאפשר לבדוק אם התיקייה מופיעה במערכת הקבצים שלנו. אם התיקייה נמצאת, החזרת הערך ה-Bollean של המתודה תהיה אמת, ואם לא, החזרת הערך תהיה שקר.

הנה עוד דוגמא של כיצד ניתן להשתמש בשיטת `Exists()` כדי לבדוק אם קובץ מסוים נמצא בתיקייה:

```C#
if(Directory.Exists("C:\\MyFolder\\myFile.txt")) 
{
    Console.WriteLine("קובץ זה קיים בתיקייה!"); 
} 
else 
{ 
    Console.WriteLine("הקובץ אינו קיים בתיקייה!"); 
}
```

הנה פלט נוסף של קוד עם שימוש בשיטת `Exists()` כדי לתמוך בטיפול בשגיאות:

```C#
try
{
    if(Directory.Exists("C:\\MyFolder")) 
    {
        Console.WriteLine("מחיקת התיקייה..."); 
        Directory.Delete("C:\\MyFolder"); 
        Console.WriteLine("התיקייה נמחקה בהצלחה!"); 
    } 
}
catch (Exception e)
{
    Console.WriteLine("אירעה שגיאה בנסיון למחוק את התיקייה: " + e.Message);
}
```

## חקר עמומה
בנוסף לשיטת `Exists()`, ישנם גם מספר שיטות אחרות שניתן להשתמש בהן כדי לבדוק אם תיקייה קיימת. לדוגמה, אפשר להשתמש בשיטת `GetDirectories()` שמחזירה מערך של כל התיקיות המכילות בתיקייה נתונ