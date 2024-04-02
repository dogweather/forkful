---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:56.854163-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA \u05D7\
  \u05D9\u05DC\u05D5\u05E5 \u05DE\u05D9\u05D3\u05E2 \u05E2\u05DC \u05D4\u05EA\u05D0\
  \u05E8\u05D9\u05DA \u05D4\u05DE\u05E7\u05D5\u05D3\u05D3 \u05D1\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA \u05D5\u05D4\u05DE\u05E8\u05EA\u05D5 \u05DC\u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05E9\u05E1\u05D1\u05D9\u05D1\
  \u05D5\u05EA \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05D6\u05D4\u05D5\u05EA\
  \ \u05D5\u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05DC\u05EA\u05E4\u05E2\u05DC \u05D5\
  \u05DC\u05E9\u05E0\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\u2026"
lastmod: '2024-03-13T22:44:40.065381-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA \u05D7\u05D9\
  \u05DC\u05D5\u05E5 \u05DE\u05D9\u05D3\u05E2 \u05E2\u05DC \u05D4\u05EA\u05D0\u05E8\
  \u05D9\u05DA \u05D4\u05DE\u05E7\u05D5\u05D3\u05D3 \u05D1\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05D5\u05D4\u05DE\u05E8\u05EA\u05D5 \u05DC\u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05E9\u05E1\u05D1\u05D9\u05D1\u05D5\
  \u05EA \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05D6\u05D4\u05D5\u05EA \u05D5\
  \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05DC\u05EA\u05E4\u05E2\u05DC \u05D5\u05DC\
  \u05E9\u05E0\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\u2026"
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## מה ולמה?
ניתוח תאריך ממחרוזת כולל את חילוץ מידע על התאריך המקודד במחרוזות והמרתו לפורמט מובנה שסביבות התכנות מזהות ויכולות לתפעל ולשנות. מתכנתים עושים זאת כדי לאפשר פעולות כמו השוואת תאריכים, חשבון, פורמטינג ולוקליזציה, שהן חיוניות לטיפול בתזמון, תגים של זמנים ונתונים היסטוריים ביעילות בתוכנה.

## איך לעשות:
ב-Fish Shell, אין לך פקודות מובנות במיוחד שתוכננו לניתוח תאריכים ממחרוזות. במקום זאת, אתה מסתמך על כלים חיצוניים כמו `date` (זמינים ב-Linux וב-macOS) או מנצל כלים צד שלישי פופולריים כמו `GNU date` לניתוח מורכב יותר. הנה איך לגשת לזה:

**שימוש ב-`date` עם Fish:**

לניתוח מחרוזת תאריך בפורמט "YYYY-MM-DD", תוכל להשתמש בפקודת `date` עם האופציה `-d` (או `--date` עבור GNU date) אחריה מופיעה המחרוזת. האופציה `+` משמשת לפורמט הפלט.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# פלט: Saturday, 01 April 2023
```

עבור macOS (שדורשת פורמט שונה עבור הדגלים `-j` ו`-f`):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# פלט: Saturday, 01 April 2023
```

**שימוש ב-GNU `date` לניתוח מורכב:**

GNU `date` היא יותר גמישה עם פורמטי מחרוזות. היא יכולה לזהות באופן אוטומטי פורמטים רבים ונפוצים של מחרוזות תאריך בלי לציין במפורש את פורמט הקלט:

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# פלט: 2023-04-01 14:00:00
```

עם זאת, כשעובדים עם מחרוזות תאריך שאולי לא יזוהו באופן אוטומטי או כשדרוש שליטה מדויקת על פורמט הקלט, הציון של פורמט הקלט עם GNU `date` אינו נתמך באופן ישיר. במקרים כאלו, שקול עיבוד מוקדם של המחרוזת או שימוש בכלי אחר שתוכנן לשגרות ניתוח תאריכים מורכבות יותר.
