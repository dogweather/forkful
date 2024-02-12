---
title:                "הסרת מרכאות ממחרוזת"
aliases:
- he/c-sharp/removing-quotes-from-a-string.md
date:                  2024-01-26T03:39:33.841904-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת מרכאות ממחרוזת ב-C# משמעותה הוצאת אותם תווים מפרכים של מרכאות כפולות (`"`) או יחידות (`'`) שמקיפים את הטקסט שלך. מתכנתים עושים זאת כדי לנקות נתונים, להכין להזנה במסד נתונים, או להפוך מחרוזות לבטוחות לעיבוד נוסף כדי שדברים לא יתפרקו כשמרכאות בודדות מופיעות במקום לא צפוי.

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
