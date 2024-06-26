---
date: 2024-01-20 18:02:15.829235-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05DE\u05D5\
  \u05D1\u05DF \u05D4\u05D4\u05D9\u05E1\u05D8\u05D5\u05E8\u05D9, \u05D0\u05D9\u05DE\
  \u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D1-HTTP \u05D4\u05D5\u05D0 \u05D0\
  \u05D7\u05D3 \u05DE\u05E9\u05D9\u05D8\u05D5\u05EA \u05D4\u05D0\u05D9\u05DE\u05D5\
  \u05EA \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D5\u05EA \u05E9\u05D4\u05D5\u05E9\
  \u05DD \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9. \u05DC\u05DE\u05E8\u05D5\u05EA \u05E9\
  \u05D4\u05D5\u05D0 \u05E0\u05D7\u05E9\u05D1 \u05DC\u05E4\u05D7\u05D5\u05EA \u05D1\
  \u05D8\u05D5\u05D7 \u05D9\u05D5\u05EA\u05E8 \u05DE\u05E9\u05D9\u05D8\u05D5\u05EA\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05DE\u05D5\u05D3\u05E8\u05E0\u05D9\u05D5\u05EA\
  \ \u05DB\u05DE\u05D5\u2026"
lastmod: '2024-04-05T22:50:54.097335-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05DE\u05D5\u05D1\u05DF \u05D4\u05D4\u05D9\u05E1\u05D8\u05D5\u05E8\
  \u05D9, \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D1-HTTP\
  \ \u05D4\u05D5\u05D0 \u05D0\u05D7\u05D3 \u05DE\u05E9\u05D9\u05D8\u05D5\u05EA \u05D4\
  \u05D0\u05D9\u05DE\u05D5\u05EA \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D5\u05EA\
  \ \u05E9\u05D4\u05D5\u05E9\u05DD \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

## איך לעשות:
```Fish Shell
# הגדרת שם המשתמש והסיסמה
set user 'shani'
set password 's3kr3t'

# יצירת ערך מקודד באימות בסיסי
set auth (echo -n "$user:$password" | base64)

# שליחת בקשת GET עם אימות בסיסי
curl -H "Authorization: Basic $auth" https://example.com/resource
```
דוגמא לפלט:
```
{
  "data": "מידע סודי שרק משתמשים מורשים יכולים לראות"
}
```

## צלילה לעומק
במובן ההיסטורי, אימות בסיסי ב-HTTP הוא אחד משיטות האימות הראשונות שהושם בשימוש. למרות שהוא נחשב לפחות בטוח יותר משיטות אימות מודרניות כמו OAuth, הוא עדיין בשימוש משום שהוא פשוט ליישום ויש תמיכה נרחבת בלקוחות HTTP שונים. חשוב לדעת שבקשות עם אימות בסיסי צריכות להתבצע תמיד דרך HTTPS כדי למנוע חשיפה של נתוני האימות. קיימות אלטרנטיבות כמו tokens, אימות דו-שלבי ו-API keys שמספקות בטחון גבוה יותר, אך לעיתים עדיין משתמשים באימות בסיסי לפעולות פשוטות או כאשר רמת הבטחון הנדרשת אינה גבוהה.

## ראה גם
- מדריך לפקודת `curl`: https://curl.se/docs/manpage.html
- מידע על אימות בסיסי ב-HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- מידע על תקן האבטחה HTTPS: https://en.wikipedia.org/wiki/HTTPS
