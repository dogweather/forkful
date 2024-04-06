---
date: 2024-01-26 03:39:33.841904-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05DE\u05D5\
  \u05E9\u05D2 \u05E9\u05DC \u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\
  \u05EA \u05D0\u05D9\u05E0\u05E0\u05D5 \u05D7\u05D3\u05E9 \u05D0\u05D5 \u05DE\u05D5\
  \u05E8\u05DB\u05D1 \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3, \u05D0\u05D1\u05DC \u05D4\
  \u05D5\u05D0 \u05E7\u05E8\u05D9\u05D8\u05D9 \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF\
  \ \u05E9\u05E1\u05D9\u05DE\u05E0\u05D9 \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DC\
  \u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\u05E9\
  \u05DE\u05E9\u05D9\u05DD \u05DC\u05D4\u05D2\u05D1\u05DC\u05EA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA. \u05DB\u05D0\u05E9\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05E2\u05DD \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA\u2026"
lastmod: '2024-04-05T21:53:40.513584-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05DE\u05D5\u05E9\u05D2 \u05E9\u05DC \u05D4\u05E1\u05E8\u05EA \u05DE\
  \u05E8\u05DB\u05D0\u05D5\u05EA \u05D0\u05D9\u05E0\u05E0\u05D5 \u05D7\u05D3\u05E9\
  \ \u05D0\u05D5 \u05DE\u05D5\u05E8\u05DB\u05D1 \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3\
  , \u05D0\u05D1\u05DC \u05D4\u05D5\u05D0 \u05E7\u05E8\u05D9\u05D8\u05D9 \u05DE\u05DB\
  \u05D9\u05D5\u05D5\u05DF \u05E9\u05E1\u05D9\u05DE\u05E0\u05D9 \u05DE\u05E8\u05DB\
  \u05D0\u05D5\u05EA \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\
  \u05D5\u05EA \u05DE\u05E9\u05DE\u05E9\u05D9\u05DD \u05DC\u05D4\u05D2\u05D1\u05DC\
  \u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA."
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## איך לעשות:
```csharp
string withQuotes = "\"Hello, World!\"";
Console.WriteLine($"Original: {withQuotes}");

// הסרת מרכאות כפולות
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"Without Double Quotes: {withoutDoubleQuotes}");

// הסרת מרכאות יחידות (בהנחה שהמחרוזת שלך הכילה אותן במקור)
string withSingleQuotes = "'Hello, World!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"Without Single Quotes: {withoutSingleQuotes}");
```

פלט:
```
Original: "Hello, World!"
Without Double Quotes: Hello, World!
Without Single Quotes: Hello, World!
```

## צלילה עמוקה
המושג של הסרת מרכאות איננו חדש או מורכב במיוחד, אבל הוא קריטי מכיוון שסימני מרכאות לעיתים קרובות משמשים להגבלת מחרוזות. כאשר מחרוזת עם מרכאות לא מוצאות מוכללת בקטע קוד או בקובץ נתונים, היא עלולה לסיים את המחרוזת באופן פרמטורי, מה שיכול לגרום לשגיאות או בעיות ביטחון כמו התקפות זריקה.

באופן היסטורי, התמודדות עם מרכאות הייתה חלק מתהליך האימות והחיטוי בטיפול בנתונים. כאשר השיטה `.Replace()` היא ישירה להוצאת מרכאות ממחרוזת פשוטה, ייתכן שתזדקק לטכניקות מתקדמות יותר כמו ביטויים רגולריים לטיפול בתרחישים מורכבים יותר, כמו מרכאות מקוננות או הסרה תנאית.

חלופות ל-`.Replace()` כוללות שיטות ממחלקת ה-`Regex` כאשר אתה זקוק לשליטה רגישה יותר או מתמודד עם תבניות ולא עם תווים קבועים. לדוגמה, `Regex.Unescape()` עשוי להיות שימושי בעת טיפול בתווים מוקפצים.

מבחינת יישום, זכור שמחרוזות ב-C# הן בלתי ניתנות לשינוי, משמע כל פעם שאתה משתמש ב-`.Replace()`, מחרוזת חדשה נוצרת. זה לא משנה קילומטרים עבור פעולות קטנות או חד-פעמיות, אבל זה משהו שכדאי לשים לב אליו מבחינת ביצועים עבור מחרוזות גדולות או רבות.

## ראה גם:
- [תיעוד המתודה String.Replace](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [ביטויים רגולריים ב-.NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [מתם הטיפול במחרוזות מאובטחת](https://www.owasp.org/index.php/Data_Validation)
