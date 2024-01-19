---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ?מה ולמה
הזחה של התאריך ממחרוזת היא התהליך שבדרך כלל מבצעת כדי להמיר מחרוזת שמייצגת את התאריך לתוך משתנה מסוג תאריך. התכנתים עושים את זה בדרך כלל כדי לאפשר פעולות על התאריכים כמו השוואה, או חישוב הפרשי זמן.

## ?איך לעשות
בעזרת המחלקה `DateTime` ב-C#, ניתן לנתח מחרוזות לתאריכים במספר דרכים. נסתכל על זה כאן:

```C#
string dateString = "12/31/2020";
DateTime parsedDate = DateTime.Parse(dateString);
Console.WriteLine(parsedDate);
```

כאשר תריצו את זה, תראו את התאריך "31/12/2020" כפלט.

באפשרותנו גם לנתח מותאם אישית של מחרוזות תאריך:

```C#
string customDateString = "31-12-2020 12:10:15";
string format = "MM-dd-yyyy HH:mm:ss";
DateTime customParsedDate = DateTime.ParseExact(customDateString, format, CultureInfo.InvariantCulture);
Console.WriteLine(customParsedDate);
```

כאן, תראו את התאריך והשעה "31/12/2020 12:10:15" כפלט.

## צלילה עמוקה
הזחת תאריכים ממחרוזות הייתה תמיד חלק חשוב של בניית תוכנה, בטח כשמדובר בממשק משתמש שבו המשתמשים הם אלו שמוזינים את התאריך כמחרוזת. זה גם משמש בעת קריאה לשירותי רשת שמחזירים תאריכים כפורמט מחרוזת.

הבדיקה של אם מחרוזת ניתנת להזחה לתאריך היא אלטרנטיבה ל-Parse ו-ParseExact. אתה יכול להשתמש ב- `DateTime.TryParse` או `DateTime.TryParseExact` שיחזירו `false` אם המחרוזת אינה תאריך תקני.

מיומנות זו, של הפיכת מחרוזת לתאריך, קיימת בכל השפות התכנות וניתן לממש אותו בצורות רבות שונות, תלוי בצורת השימוש המדויקת שהיא נדרשת.

## ראה גם
[תיעוד Microsoft על DateTime.Parse](https://docs.microsoft.com/he-il/dotnet/api/system.datetime.parse?view=netframework-4.8)  
[תיעוד Microsoft על DateTime.ParseExact](https://docs.microsoft.com/he-il/dotnet/api/system.datetime.parseexact?view=netframework-4.8)  
[מדריך סטאק אוברפלו על ניתוח מחרוזות לתאריכים](https://stackoverflow.com/questions/919244/converting-a-string-to-datetime)