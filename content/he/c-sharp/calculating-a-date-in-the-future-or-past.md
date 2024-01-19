---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "C#: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?

חישוב תאריך בעתיד או בעבר הוא תהליך של מניפולציה על תאריך כדי למצוא תאריך אחר המתייחס לו. מתכנתים מפעילים את זה לצורך עיבוד היסטוריה, תזמון, תכנון, ועוד.

## כיצד לעשות:

על מנת לחשב תאריך בעתיד או בעבר, אנו משתמשים בכלי DateTime של C#. שימו לב להדגמה הבאה.

```C#
DateTime todaysDate = DateTime.Now;
DateTime futureDate = todaysDate.AddDays(10);
DateTime pastDate = todaysDate.AddDays(-5);
Console.WriteLine("Today's date: " + todaysDate.ToShortDateString());
Console.WriteLine("Future date: " + futureDate.ToShortDateString());
Console.WriteLine("Past date: " + pastDate.ToShortDateString());
```

וזו תהיה הפלט:

```C#
Today's date: 10/31/2023
Future date: 11/10/2023
Past date: 10/26/2023
```

## הפוקוס העמוק:
### היסטוריה
החישוב של תאריכים בעתיד או בעבר הפך לכלי נפוץ מאז הוצגה לראשונה בשפות תכנות, מאגרי מידע ולמעשה כל ממשק שיש לו את הצורך לחשב את מרווח הזמן בין תאריכים. זהו כלי קריטי עבור כרטיסי בנק, שרותים משלוחים, ומערכות התזמון.

### חלופות
בנוסף למתודת `AddDays()`, ישנן מתודות נוספות כמו `AddHours()`, `AddMinutes()`, `AddMonths()`, `AddYears()` ועוד, אשר מאפשרות למתכנתים להוסיף או להחסיר ימים, שעות, דקות, חודשים, ושנים מתאריך.

### פרטי המימוש
חישוב תאריך בעתיד או בעבר מתבצע על-ידי اתחול התאריך המקורי, ואז הוספה או הפחתה של כמות הימים, החודשים, השנים, השעות, הדקות, לדוגמה. אפשרות אחרת היא שימוש בשיטות של `TimeSpan`.

## ראה גם
* DateTime: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0
* TimeSpan: https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-6.0
* מאגר מידע מקוון על C#: https://www.c-sharpcorner.com/learn/learn-c-sharp-programming