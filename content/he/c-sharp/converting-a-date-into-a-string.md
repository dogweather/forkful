---
title:    "C#: המרת תאריך למחרוזת"
keywords: ["C#"]
---

{{< edit_this_page >}}

מדוע: 
המרת תאריך למחרוזת היא תהליך חשוב בתכנות C#, מכיוון שהמחרוזת היא דרך נוחה ונרחבת יותר להצגת תאריך מאשר ערך נומרי. בנוסף, מומלץ להשתמש במחרוזת כאשר אנחנו רוצים להכיל מידע נוסף על התאריך בנוסחא מובנה.

איך לבצע:
ניתן להמיר תאריך למחרוזת בעזרת הפונקציה ToString (). הנה דוגמא לקוד ב-C#:

```C#
DateTime date = new DateTime(2020, 12, 31);
string dateString = date.ToString();
Console.WriteLine(dateString);

// Output: 31/12/2020 12:00:00 AM
```

דילוג עמוק:
הפונקציה ToString () מאפשרת לנו להציג את התווים הנוספים במחרוזת, כגון שם החודש, סימן היום הראשון וכו'. ניתן להשתמש בפורמטים שונים בפונקציה כדי להציג את התאריך בצורה מותאמת אישית.

לדוגמא:

```C#
DateTime date = new DateTime(2020, 12, 31);
string dateString = date.ToString("dd MMMM yyyy");
Console.WriteLine(dateString);

// Output: 31 דצמבר 2020
```

בנוסף, ניתן להשתמש בפונקציה להפיכת התאריך למחרוזת בלי התווים הנוספים, כדי להציג רק את היום והחודש כמחרוזת מספרית.

עמודות לכתבות נוספות:

ראו גם:
- תיעוד על הפונקציה ToString () באתר הרשמי של C#: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring
- הספרייה של Microsoft להפיכת תאריך למחרוזת: https://www.microsoft.com/en-us/globalization/dfps.mspx
- פוסט נוסף בבלוג שלנו על השימוש בפונקציה ToString () להפיכת תאריך למחרוזת: [קישור לפוסט]