---
title:                "עבודה עם JSON"
date:                  2024-02-03T19:21:56.661844-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
