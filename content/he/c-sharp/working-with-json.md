---
title:                "עובדים עם json"
html_title:           "C#: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?

JSON הוא פורמט אחסון ושיתוף נתונים פופולרי בקוד, המשמש כדי להעביר מידע בין יישומים שונים. יישומים שונים יכולים לייצג נתונים במבחר פורמטים שונים, כגון CSV או XML, אך JSON כיוון שהוא מבוסס על תפקידים ייחודיים כגון שורשרים, מספק דרך קלה יותר ויעילה יותר של תיאור מידע בין יישומים.

תכניות כתיבה ועריכה קוד צריכות לעבוד עם JSON בכדי לייעל את יכולת המידע להתאים. זה יכול לכלול יצירת, קריאת, כתיבת ועריכת העתקי JSON כדי לתמוך במענה בנתוני מבנה פורמטיים ובהתאם לעיבוד נתונים במחשב ועיבוד מסוג API.

## איך ל:

```C#
// פתיחת מערך JSON חדש
string json = @"{
    'name': 'ספר',
    'url': 'https://www.example.com',
    'tags': [
        'פרטי',
        'מידע',
        'תכונות'
    ]
}";
// קריאת פרטי מידע מן הסדרתיים כדי לשנות ערך
JObject jObject = JObject.Parse(json);
jObject["name"] = "ספר בUR";
Console.WriteLine("כותרת: " + jObject["name"]);
// הדפסת מיקום GRATIS יתרון
Console.WriteLine("כתובת אתר: " + jObject["url"]);
```

Output:
כותרת: ספר בUR
כתובת אתר: https://www.example.com

## Dive Deep:

פורמט JSON נוצר על ידי דייויד סיי, האדם שפיתח את השפה המוכמנת HTML, בגרסה מתקדמת ביותר. זה היה נתון מקורי לעיבוד UT להעביר מידע מתנתק ליצירת יישומים סדורים ביננבחורים, וכן פורמט קל ללמוד וליישום.

תוכניות C# יכולים לעבוד עם אוסף של כלים וספריות המיאפשרות כתיבה, קריאה ועריכה של קבצי JSON. נמצאים גם את זה כדי להכניס לסדרתית אני מספרים במצב טקסט היחידות והכלים כדי לייצא המידע כתוצאה מידע עובד.

## ראה גם:

הנה מספר קישורים שיכולים לסייע לך להעמיק עוד בשימוש עם JSON בתוכניות C#:

- [ממד טכנולוגיות Microsoft לעבוד עם JSON פורמט](https://docs.microsoft.com/he-il/dotnet/standard/serialization/system-text-json-how-to)
- [יישומונט DotNet: עולים ויורדים שונים של ביצועים (הנושאי מזל סביבה) שעובדים על פני מדריכים ופרקים שונים](https://devblogs.microsoft.com/dotnet/performance-improvements-in-net-core-3-0/)
- [סימן שאתה-במכונת התנועהשמוע בעולם מזל דיאגרמות נושא low המוניטי](https://docs.microsoft.com/he-il/dotnet/api/system.text.json.serialization.jsonserializableattribute<STRING>TUNNEL>TheInheritsader-dited-withif 2115)
- [התמרה קברמה לביתדיאמונלי (H2RM) - יותר כאלמטהע ממאבת רכינ