---
title:    "C#: בדיקת קיומו של תיקייה במחשב"
keywords: ["C#"]
---

{{< edit_this_page >}}

# למה

בשפת סי-שארפ (C#) קיימת אפשרות לבדוק אם תיקייה קיימת במערכת הקבצים. ניתן להשתמש בפונקציה המובנית "Directory.Exists" לצורך בדיקה כזו. מטרת הפוסט הזה היא ללמוד כיצד לבדוק אם תיקייה קיימת בעזרת קוד דוגמה פשוט והסברים נוספים על הנושא.

# איך לבדוק אם תיקייה קיימת ב-C#

תחילה, נצרף את המרחב השמור "System.IO" בשביל להשתמש בפונקציה "Directory.Exists":

```C#
using System.IO;
```

כעת, פשוט נקרא לפונקציה "Directory.Exists" עם הנתיב של התיקייה אותה אנחנו רוצים לבדוק אם קיימת:

```C#
string path = @"C:\Users\JohnDoe\Documents";
if (Directory.Exists(path))
{
    Console.WriteLine("The directory exists!");
}
```

הפלט של הקוד הנ"ל יהיה "The directory exists!" אם התיקייה קיימת בנתיב שציינו, או שכללוא, לא יתווסף תנאי במידה והתיקייה לא קיימת. ניתן לשנות את הנתיב לכל נתיב אחר כפי שמתאים לכם.

ניתן גם להשתמש במתודת החזרה של "Directory.Exists" על מנת לבחון האם התיקייה קיימת או לא, ולפעול בהתאם:

```C#
string path = @"C:\Users\JohnDoe\Documents";
bool directoryExists = Directory.Exists(path);

if (directoryExists)
{
    Console.WriteLine("The directory exists!");
}
else
{
    Console.WriteLine("The directory does not exist.");
}
```

# יוצא דופן - בדיקה חלקה של תיקיות לפי תבנית

אפשר להשתמש בפונקצית "Directory.Exists" גם כדי לבדוק אם תיקייה קיימת בתוך תיקייה אחרת, תוך שימוש בתבנית או בפקודות מתאימות. לדוגמה, נדמיין שלנו יש תיקייה יצאת דופן במערכת הקבצים שלנו, והיינו רוצים לבדוק כמה תיקיות בתוכה קיימות וכמה לא. למעשה, הנה פעולת החיפוש הכ