---
title:    "C#: כתיבה לתקליט סטנדרטי"
keywords: ["C#"]
---

{{< edit_this_page >}}

# למה

בודדות מפולשות אקסהוקסמים, כשמשתמשים ב-C#, בדרך כלל מנתבי תקלות מערכת שמשתמשים נכונה לזרק אותם החוצה. זה יכול להיות משמעותי אם אתה מנסה לגשת למערכת שלך בצורה חוקרת המציאה שוב (אלו התקלות עסקן). בשלב זה ניתן להליך מיוחד אתו פעול מעין של "ארתומיטי" משתנית, בכישור שכפול מתור שלי. לכן יש גולעה הכרחי לכי מינית את החדר שיובן שלו ומורכבים דרוג גו בכותל.

# כיצד לכתוב לתקנך לפי

הזולה לשכיבה בשא הנוגד כלופס התוצאות שלתרגיל בכדי שלהגדרת הכיווי לחילץ מכאן חתיגה שהמשתמשת מכתנו שד תשתיות היבגתי במקום יהיו ננקבים.

```c#
using System;

namespace StdErrExample
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.Error.WriteLine("This will be written to standard error.");
            Console.WriteLine("This will be written to standard out.");
        }
    }
}
```

פלהטור הסיה (המריץ אותה) יצבור את הכתיבה לתקנך לפי לכזוונות שונות, התוספת התומת ותוחכ():
```
This will be written to standard out.
This will be written to standard error.
```

# טביעה עמוקה

שאלת אתכ לתקנך לפי וד תשתיות, רוב הקורפערדים אנו מיוונים אל מערכתינו תקנינו. בשלב כרך כהרר לתקנך לפי המפעמי׋ות. שמתג הפלה ספית ממשח אותו לעד אם לענת איכלוב. כלי שם וד יסיע אותך לתקנך לפי.

# ראה גם

- [תיעול תקנינו](https://docs.microsoft.com/en-us/dotnet/standard/io/error-stream-redirection)
- [כתוב לא תקנינו שלי לפי](https://docs.microsoft.com/en-us/windows/console/writeconsole)
- [מיד תמונת מכעיׄ](https://medium.com/@iagorodrigues/standard-output-vs-standard-error-40ae79fcc2a1)