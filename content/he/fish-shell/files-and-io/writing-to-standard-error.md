---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:56.092569-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05D1-Fish Shell \u05D4\
  \u05D9\u05D0 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D4\u05DB\u05D5\u05D5\u05D9\
  \u05DF \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D0\
  \u05D5 \u05E2\u05E7\u05D9\u05D1\u05D5\u05EA \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\
  \u05D4\u05E4\u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout).\
  \ \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05E0\u05D9\u05EA\
  \u05DF\u2026"
lastmod: '2024-02-25T18:49:38.319260-07:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05D1-Fish Shell \u05D4\u05D9\
  \u05D0 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D4\u05DB\u05D5\u05D5\u05D9\u05DF\
  \ \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D0\u05D5\
  \ \u05E2\u05E7\u05D9\u05D1\u05D5\u05EA \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\u05D4\
  \u05E4\u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout). \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05E0\u05D9\u05EA\u05DF\
  \u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה לשגיאה סטנדרטית (stderr) ב-Fish Shell היא על מנת להכווין הודעות שגיאה או עקיבות בנפרד מהפלט הסטנדרטי (stdout). תכנתים עושים זאת כדי לוודא שניתן לזהות בקלות, לנהל או להפנות את מידע השגיאה, מה שמקל על תהליכים של ניפוי באגים ורישום.

## איך לעשות:

ב-Fish Shell, תוכלו לכתוב ל-stderr על ידי הפניית הפלט שלכם באמצעות `>&2`. הנה דוגמה בסיסית:

```fish
echo "This is an error message" >&2
```

פקודה זו פשוט מדפיסה הודעה ל-stderr במקום ל-stdout. אם הייתם כותבים סקריפט שמוציא הודעות רגילות וגם הודעות שגיאה, אולי הייתם עושים משהו כזה:

```fish
echo "Starting the process"
echo "An error occurred" >&2
echo "Process completed"
```

פלט לדוגמה אם תריצו את הסקריפט ותפנו את stderr לקובץ:

```
Starting the process
Process completed
```

ההודעה על השגיאה לא הייתה מופיעה בפלט הסטנדרטי אלא תמצא בקובץ אליו הפניתם את ה-stderr.

בסצנריות הדורשות טיפול בשגיאות או רישום מתוחכם יותר, Fish אינו מגיע עם ספריות מובנות שתוכננו במפורש לכך. עם זאת, תוכלו לנצל כלים חיצוניים או לכתוב פונקציות שיסייעו. לדוגמה, יצירת פונקציית רישום פשוטה עשויה להיראות כך:

```fish
function log_error
    echo $argv >&2
end

log_error "This is an advanced error message"
```

פונקציה זו `log_error` תקח כל מחרוזת שתיתנו לה ותכתוב אותה ל-stderr. שימוש בפונקציות כמו זו יכול לעזור לשמור על טיפול השגיאות שלכם נקי ועקבי ברחבי הסקריפטים שלכם.
