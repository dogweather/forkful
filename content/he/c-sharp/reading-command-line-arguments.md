---
title:    "C#: קריאת ארגומנטים בשורת פקודה"
keywords: ["C#"]
---

{{< edit_this_page >}}

## למה
בפיתוח תוכניות, עליך לדעת איך לטפל בקלטים מהשורת פקודה. זהו כלי חיוני כאשר אתה רוצה לתת למשתמשים את האפשרות להעריך ולשנות את התוכנית שלך. במאמר הזה, נלמד כיצד לקרוא ולהתמודד עם קלט מהשורת פקודה בשפת סי שארפ.

## איך לעשות זאת
נתחיל עם קוד פשוט של שורת פקודה להדגמה:

```C#
static void Main(string[] args)
{
    string[] arguments = Environment.GetCommandLineArgs();

    for (int i = 0; i < arguments.Length; i++)
    {
        Console.WriteLine("Argument {0}: {1}", i + 1, arguments[i]);
    }
}
```

בקוד זה, אנחנו משתמשים בפונקציה `Environment.GetCommandLineArgs()` כדי לקבל את כל הקלטים מהשורת פקודה כמערך של מחרוזות. אחר כך, באמצעות לולאת `for` אנחנו מדפיסים את כל הקלטים בכתב גותי לצורך הדגמה.

הנה כמה דוגמאות לקלטים ולפלט של הקוד הנ"ל:

קלט: `myProgram.exe`  
פלט:

```
Argument 1: myProgram.exe
```

קלט: `myProgram.exe 1 2 3`  
פלט:

```
Argument 1: myProgram.exe
Argument 2: 1
Argument 3: 2
Argument 4: 3
```

## חקירה מעמיקה
בנוסף לכתיבת הקלטים למסך, ניתן גם לעבוד עם הקלטים כקלט לתכנית. זה יכול להיות שימושי כאשר אתה רוצה לשנות את הפעולה של התוכנית לפי הקלט שהתקבל.

לדוגמה, אנחנו נבנה תוכנית פשוטה שמחשבת את מכפלת שני מספרים שנתונים כקלט מהשורת פקודה:

```C#
static void Main(string[] args)
{
    if (args.Length == 3)
    {
        int num1 = Convert.ToInt32(args[1]);
        int num2 = Convert.ToInt32(args[2]);
        Console.WriteLine("The product is: {0}", num1 * num2);
    }
    else
    {
        Console.WriteLine("Please input two numbers as command line arguments.");
    }
}
```

בתוכנית זו, אנו בודקים את אורך המערך שמכיל את הקלטים מ