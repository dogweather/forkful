---
title:                "המרת תאריך למחרוזת"
aliases: - /he/c-sharp/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:12.500137-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת היא פעולה שבה אתה לוקח ערך תאריך ושם וממיר אותו לטקסט. מתכנתים עושים את זה כדי להציג תאריכים למשתמשים בפורמט קריא או להפוך אותם למתאימים לאחסון במאגרי נתונים.

## איך לעשות:
ב-C#, אתה יכול להשתמש במחלקת `DateTime` כדי לעבוד עם תאריכים, ובמתודה `ToString` כדי להמיר תאריך למחרוזת.

```C#
DateTime now = DateTime.Now;
// המרה עם פורמט ברירת מחדל
string defaultFormat = now.ToString();
Console.WriteLine(defaultFormat); // יוצא לדוגמה: "4/12/2023 8:31:52 AM"

// המרה עם פורמט מותאם אישית
string customFormat = now.ToString("dd/MM/yyyy");
Console.WriteLine(customFormat); // יוצא לדוגמה: "12/04/2023"
```

השתמש בקוד לעיל כדי להמיר תאריכים למחרוזות. החלף את הפורמט למה שהכי מתאים לך.

## עיון מעמיק:
ההמרה של תאריכים למחרוזות אינה תמיד פשוטה כמו שנראה. ב-C# ישנם פורמטים רבים, והמערכת תומכת בלוקליזציה, כלומר היא מאפשרת להציג תאריכים בפורמט הנכון לאזורים שונים מבלי לדעת בהכרח פרטים על הפורמט.

בעבר, מתכנתים השתמשו בספריות צד שלישי כמו `NodaTime` לעיבוד תאריכים, שהיא מעט יותר ורסטילית מ-`DateTime`. אולם, עם השנים `DateTime` זכתה לשיפורים, ובחלק מהמקרים אין צורך יותר בספריות אלו.

על המתכנתים להיות מודעים ל"מלכודות" כמו ניהול אזורי זמן, תאריכים עבריים או שינויים בסטנדרטים.

## ראה גם:
- [Microsoft Docs - עבודה עם תאריכים וזמנים](https://docs.microsoft.com/en-us/dotnet/standard/datetime/)
- [Microsoft Docs - מחלקת DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [ניהול פורמטים של תאריכים וזמנים ב-C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Stack Overflow - שאלות ותשובות על המרת תאריכים ומחרוזות](https://stackoverflow.com/questions/tagged/datetime+string-formatting+c%23)
