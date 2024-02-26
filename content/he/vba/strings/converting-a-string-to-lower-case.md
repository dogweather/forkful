---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:57.079945-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05D4\u05E4\u05D9\u05DB\u05D4 \u05E9\u05DC \u05DB\u05DC \u05D4\
  \u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D2\
  \u05D3\u05D5\u05DC\u05D5\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05E9\u05E7\u05D5\u05DC\u05D5\u05EA\u05D9\u05D4\u05DF \u05D1\u05D0\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\
  \u05D4 \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA\
  \ \u05EA\u05DB\u05E0\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA, \u05DB\u05D5\u05DC\
  \u05DC \u05E0\u05E8\u05DE\u05D5\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD,\u2026"
lastmod: '2024-02-25T18:49:37.274908-07:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05D4\u05E4\u05D9\u05DB\u05D4 \u05E9\u05DC \u05DB\u05DC \u05D4\u05EA\
  \u05D5\u05D5\u05D9\u05DD \u05D1\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D2\u05D3\
  \u05D5\u05DC\u05D5\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05E9\
  \u05E7\u05D5\u05DC\u05D5\u05EA\u05D9\u05D4\u05DF \u05D1\u05D0\u05D5\u05EA\u05D9\u05D5\
  \u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\u05D4\
  \ \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05EA\
  \u05DB\u05E0\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA, \u05DB\u05D5\u05DC\u05DC\
  \ \u05E0\u05E8\u05DE\u05D5\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD,\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

המרת מחרוזת לאותיות קטנות כוללת הפיכה של כל התווים באותיות גדולות במחרוזת לשקולותיהן באותיות קטנות. תהליך זה חיוני למשימות תכנות שונות, כולל נרמול נתונים, השוואות לא תלויות ברישיות ושיפור עקביות קלט משתמש.

## איך לעשות:

ב-Visual Basic for Applications (VBA), המרת מחרוזת לאותיות קטנות היא פשוטה באמצעות הפונקציה `LCase`. פונקציה זו לוקחת מחרוזת כקלט ומחזירה מחרוזת חדשה עם כל התווים באותיות גדולות המומרים לאותיות קטנות. הנה דוגמה בסיסית שממחישה את זה:

```basic
Dim originalString As String
Dim lowerCaseString As String

originalString = "Hello, World!"
lowerCaseString = LCase(originalString)

Debug.Print lowerCaseString ' פלט: hello, world!
```

ניתן גם להשתמש ב-`LCase` ישירות בהשוואות או בהקצאות לקוד ממוזער:

```basic
If LCase(userInput) = "yes" Then
    Debug.Print "User said yes"
End If
```

הדוגמה השנייה ממחישה כיצד לטפל בקלט משתמש באופן שאינו תלוי ברישיות על ידי המרת הקלט לאות קטנה לפני ההשוואה.

## עיון נוסף

הפונקציה `LCase` היא בסיס למניפולציה של מחרוזות ב-VBA והיא תכונה מרכזית מאז היווסדות השפה. היא מפשטת משימות של המרת רישיות, שהן נפוצות בפרסור נתונים ובעיבוד קלט משתמש. אף על פי ש-`LCase` מספקת ביעילות את הצורך להמרת תווים לאותיות קטנות ביישומים שונים, חשוב גם לזהות את הגבלותיה והחלופות לה.

לדוגמה, בעוד ש-`LCase` עובדת ללא תקלה עבור האלפבית האנגלי, טיפול בשפות עם חוקי רישיות מורכבים יותר עלול לדרוש שיקולים נוספים או שימוש בפונקציה `StrConv` עם הגדרות מיקום המתאימות להמרת רישיות.

בנוסף, כאשר מתבצעת מעבר משפות כמו Python, שבה נעשה שימוש ב-`str.lower()`, או JavaScript, עם ה-`string.toLowerCase()` שלה, תכנתי עשויים למצוא את `LCase` פשוטה אך עליהם לזכור את החידושים הייחודיים ל-VBA, כמו העדר יכולת שרשור שיטות.

לסיכום, תוך כדי שקיימות חלופות חדשות ואולי יותר חזקות בשפות אחרות, `LCase` נשארת פונקציה אמינה ופשוטה לשימוש להמרת מחרוזות לאותיות קטנות ב-VBA, והיא משתלבת היטב בתרשים הסינטקס והפונקציונליות הכללי של השפה.
