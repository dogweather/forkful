---
title:                "C#: חישוב תאריך בעתיד או בעבר"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# למה

חישוב תאריך בעתיד או בעבר יכול להיות שימושי לכמה מטרות שונות, כגון ייצוג אירועים מפותחים או לתכנות תוכניות עתידיות.

# איך לעשות זאת

בהשתמש בשפת C# ניתן ליצור תכנית פשוטה המחשבת תאריך בעתיד או בעבר על פי הקלט של המשתמש. לדוגמה:

```C#
// חישוב תאריך בעתיד
DateTime futureDate = DateTime.Today.AddYears(1);
Console.WriteLine("תאריך בעתיד: " + futureDate); // תאריך בעתיד: 30/06/2022

// חישוב תאריך בעבר
DateTime pastDate = DateTime.Today.AddYears(-1);
Console.WriteLine("תאריך בעבר: " + pastDate); // תאריך בעבר: 30/06/2020
```

ניתן לעדכן את הקוד כך שיקבל קלט מהמשתמש ויחשב תאריך בהתאם לקלט שנקבל. לדוגמה, אם משתמש מזין את השנה, החודש והיום המבוקשים, ניתן להשתמש בפקודת `DateTime.Parse` כדי ליצור תאריך מהמשתנים הללו ולהחשיב את התאריך בעתיד או בעבר.

# נסיעה עמוקה

כדי להיות יעילים יותר בחישובי תאריך, ניתן להתמקד יותר בפקודות ספציפיות כמו `DateTime.Today` ו- `DateTime.Now` המאפשרות לנו לקבל את התאריך הנוכחי, ולא רק לחשב תאריך בעתיד או בעבר. ניתן גם ליצור תכניות המחשבות תאריך עתידי או בעבר על פי הקלט שניתן לראות על ידי המשתמש.

# ראה גם

- [כתיבת תאריך בשפת C#](https://www.w3schools.com/cs/cs_dates.asp)
- [התאריכים והמחשבים](http://www.cs.columbia.edu/~allen/F17/NOTES/dates.pdf)
- [תיעוד של String.Format](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-5.0)