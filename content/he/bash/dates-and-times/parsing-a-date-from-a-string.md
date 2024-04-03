---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:40.639860-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Bash \u05DB\u05D5\u05DC\u05DC \u05D0\
  \u05EA \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D5\u05D4\u05DE\u05E8\u05EA \u05DE\u05D9\
  \u05D3\u05E2 \u05E2\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DE\u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\
  \u05DD \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D0\u05D5\u05EA\u05D5 Bash \u05D9\
  \u05DB\u05D5\u05DC \u05DC\u05EA\u05E4\u05E2\u05DC \u05D0\u05D5 \u05DC\u05D4\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05D5 \u05DC\u05EA\u05D4\u05DC\u05D9\u05DB\u05D9\u05DD\
  \ \u05E0\u05D5\u05E1\u05E4\u05D9\u05DD. \u05D6\u05D4\u05D5 \u05D3\u05E8\u05D9\u05E9\
  \u05D4\u2026"
lastmod: '2024-03-13T22:44:39.640557-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Bash \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D5\u05D4\u05DE\u05E8\u05EA \u05DE\u05D9\u05D3\
  \u05E2 \u05E2\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DE\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\u05DD\
  \ \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D0\u05D5\u05EA\u05D5 Bash \u05D9\u05DB\
  \u05D5\u05DC \u05DC\u05EA\u05E4\u05E2\u05DC \u05D0\u05D5 \u05DC\u05D4\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05D5 \u05DC\u05EA\u05D4\u05DC\u05D9\u05DB\u05D9\u05DD \u05E0\
  \u05D5\u05E1\u05E4\u05D9\u05DD."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות:
Bash בפני עצמו מוגבל מאוד ביכולות ניתוח תאריכים ישירות, ולעיתים קרובות מסתמך על כלים חיצוניים כמו `date` ו-`awk` למניפולציה מתוחכמת יותר. הנה איך אפשר לנתח פורמט מסוים ולאחר מכן להשתמש בו עם פקודת `date` כדי להמיר אותו או לבצע פעולות.

**דוגמא 1:** חילוץ מחרוזת תאריך והמרתה לפורמט אחר.

נניח יש לך תאריך בפורמט `yyyy-mm-dd` ואתה רוצה להמירו ל-`dd-mm-yyyy`.

```bash
original_date="2023-04-01"
formatted_date=$(date -d $original_date '+%d-%m-%Y')

echo $formatted_date
```

**דוגמת פלט:**
```
01-04-2023
```

דוגמא זו משתמשת בפקודת `date` עם אפשרות ה-`-d` לציון מחרוזת התאריך הקלט, ו-`+%d-%m-%Y` לפורמט הפלט.

**דוגמא 2:** שימוש ב-`awk` לניתוח תאריך משורת טקסט מובנית והמרתו.

בהנחה שיש לך שורת קובץ יומן:

```
2023-04-01 12:00:00 User logged in
```

אפשר לחלץ ולהמיר את חלק התאריך באמצעות `awk` ו-`date`.

```bash
log_line="2023-04-01 12:00:00 User logged in"
date_part=$(echo $log_line | awk '{print $1}')
formatted_date=$(date -d $date_part "+%A, %B %d, %Y")

echo $formatted_date
```

**דוגמת פלט:**
```
Saturday, April 01, 2023
```

דוגמא זו משתמשת ב-`awk` כדי לפצל את שורת היומן ולחלץ את חלק התאריך (`$1` מייצג את השדה הראשון המופרד ברווח), ולאחר מכן משתמשים ב-`date` כדי להמיר אותו.

### שימוש בכלים של צד שלישי
לניתוח מורכב יותר או כאשר מתמודדים עם מגוון רחב של פורמטים של תאריכים, כלים של צד שלישי כמו `dateutils` יכולים להיות מועילים מאוד.

**דוגמא עם `dateutils`:**

בהנחה שיש לך מחרוזת תאריך בפורמט לא סטנדרטי, למשל, `April 01, 2023`.

```bash
original_date="April 01, 2023"
formatted_date=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_date)

echo $formatted_date
```

**דוגמת פלט:**
```
2023-04-01
```

פקודה זו משתמשת ב-`dateconv` מ-`dateutils`, תוך ציון פורמט הקלט עם `-i` והפורמט הרצוי לפלט עם `-f`. `dateutils` תומכת במגוון רחב מאוד של פורמטים של תאריכים ושעות, מה שהופך אותה לכלי מאוד גמיש למשימות ניתוח תאריכים בסקריפטים של Bash.
