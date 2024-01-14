---
title:    "C#: קריאת ארגומנטים משורת הפקודה"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

מה המניע שמעורבו יש לקרוא ארגומנטים של שורת פקודה?

קריאת ארגומנטים של שורת פקודה נחשבת לכלי חשוב ביותר בתוך עולם התכנות ומאפשרת יצירת מכשירים על מנת לרוץ באמצעות פרמטרים מקבילים. כאשר הערכים היו שמונים יחד עם קוד המכשיר עצמו, כאשר אין להם חשיבה על הקוד עצמו.

## שיטות

### ישנם שלושה דרכים אנחנו נוכל לקרוא שורת פקודה, הנה דוגמא:

```C#
// דוגמא 1: משתמשים ב-main כדי לקבל את כל הארגומנטים המועברים
static void Main(string[] args)
{
    for(int i=0; i<args.Length;i++)
    {
        Console.WriteLine(args[i]);
    }
}

// דוגמא 2: משתמשים ב-'Execute' כדי להריץ את הקוד שלכם באמצעות הארגומנטים המתאימים
static void Main()
{
    string[] args = Environment.GetCommandLineArgs();
    
    // גרסה זו כרוכב שיעיד לעשות כל טיפולים על ארגומנטים שחבילת 
    // העומס של אלמונים הנשאיים נבחר לך
    foreach (string s in args)
    {
        Console.WriteLine(s);
    }
}
```

### דוגמא 3: העבור ל-m כדי לקבל גישה מיידית לטיפולים קוד־הארד לטיפול בשמות אחרים, כמו גישהיוך

```C#
// קבל את הארגומנטים המתאימים ומאחסן את הארגומנטים שאתם צריך על מנת לטפל בהם
static int Main(string[] args)
{
    // טיפול בפרמטרים שמקורים במקרה של העורך
    string s = Array.Find(args, each => each.StartsWith("--ip"));
    string[] hosts = s.Split(' ');
    
    // אם הם צריכים להתאים ל־סרט לוחמות הניקטנחאל תפוק NULL וכאשר
    if (hosts == null || hosts.Length!=2)
    {
        Console.WriteLine("Please specify two hosts.");
    }
    return 0;
}
```

## חקירה עמוקה

יותר על קריאת ארגומנטים של שורת פק