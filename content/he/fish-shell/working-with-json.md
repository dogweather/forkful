---
title:                "עבודה עם JSON"
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON מתייחסת לניתוח ויצירת מידע בתסדיר JSON, פורמט נתונים פופולרי באינטרנט. תכניתנים משתמשים בו מכיוון שהוא קל לקריאה ופשוט לעיבוד בשפות תכנות רבות.

## איך לעשות:
ב-Fish Shell, תוכלו להשתמש בכלים כמו `jq` לעבודה עם JSON. דוגמאות לפקודות ופלט:

```Fish Shell
# ניתוח JSON והשגת ערך של מפתח (key)
echo '{"name": "Yonatan", "age": 30}' | jq '.name'
# פלט: "Yonatan"

# תיקון פורמט של JSON
echo '{"name":"Yonatan","age":30}' | jq .
# פלט:
# {
#   "name": "Yonatan",
#   "age": 30
# }
```

## לעומק:
JSON (JavaScript Object Notation) הוא תסדיר שהתפתח משפת JavaScript, אך הוא תומך במגוון שפות. החלופות כוללות XML וYAML, אבל JSON נמצא בשימוש נרחב בגלל הקלות לשימוש בו בפרונט-אנד ובק-אנד. כאשר עובדים עם Fish Shell, `jq` הוא כלי חזק שמממש יכולות ניתוח ועריכה של JSON.

## קישורים נוספים:
- [דוקומנטציה של jq](https://stedolan.github.io/jq/manual/)
- [מדריך למתחילים על JSON](https://www.w3schools.com/js/js_json_intro.asp)
- [תיעוד Fish Shell](https://fishshell.com/docs/current/index.html)
