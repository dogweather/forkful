---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:56.661844-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05D1\u05EA\u05DB\u05E0\
  \u05D5\u05EA Bash \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05E2\u05E0\u05D5\u05D7\
  , \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05E0\u05EA\
  \u05D5\u05E0\u05D9 JSON \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DE\u05E9\u05D5\u05E8\
  \u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\
  \u05D1 \u05D1\u05D0\u05D5\u05E4\u05DF \u05D7\u05DC\u05E7 \u05E1\u05E7\u05E8\u05D9\
  \u05E4\u05D8\u05D9\u05DD \u05E2\u05DD \u05DE\u05DE\u05E9\u05E7\u05D9\u2026"
lastmod: '2024-03-11T00:14:13.145479-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05D1\u05EA\u05DB\u05E0\
  \u05D5\u05EA Bash \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05E2\u05E0\u05D5\u05D7\
  , \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05E0\u05EA\
  \u05D5\u05E0\u05D9 JSON \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DE\u05E9\u05D5\u05E8\
  \u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\
  \u05D1 \u05D1\u05D0\u05D5\u05E4\u05DF \u05D7\u05DC\u05E7 \u05E1\u05E7\u05E8\u05D9\
  \u05E4\u05D8\u05D9\u05DD \u05E2\u05DD \u05DE\u05DE\u05E9\u05E7\u05D9\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON בתכנות Bash כוללת פענוח, חילוץ ושינוי נתוני JSON ישירות משורת הפקודה. תכנתים לעיתים קרובות עושים זאת כדי לשלב באופן חלק סקריפטים עם ממשקי API של אתרי אינטרנט ותסדירי החלפת מידע מודרניים, הופך את הסקריפטינג של Bash לחזק ורלוונטי יותר באקוסיסטם שלוקח JSON בחשיבות רבה.

## איך לעשות:
Bash עצמו חסר יכולות פרסור JSON מובנות, אך `jq` הוא מעבד שורת פקודה JSON חזק שממלא את החסר. הנה איך להשתמש בו:

**קריאת קובץ JSON:**

דוגמה ל`data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

לקרוא ולחלץ את השם מתוך הקובץ JSON:
```bash
jq '.name' data.json
```
פלט:
```
"Jane Doe"
```

**שינוי נתוני JSON:**

עדכון העיר ל"Los Angeles" וכתיבה חוזרת לקובץ:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**פרסור JSON מתוך משתנה:**

אם יש לך JSON במשתנה של Bash, `jq` יכול עדיין לעבד אותו:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
פלט:
```
"John Doe"
```

**עבודה עם מערכים:**

נתון מערך של פריטים בJSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

לחלץ את הפריט השני (האינדקסים מתחילים מ-0):
```bash
jq '.items[1]' data.json
```
פלט:
```
"banana"
```

לפעולות מורכבות וסינון נוסף, ל-`jq` יש מדריך מלא ומדריכים מקוונים, הופך אותו לכלי גמיש לכל הצרכים שלך ב-Bash/JSON.
