---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON הוא פורמט נתונים, פופולרי להחלפת מידע באינטרנט בזכות קריאותו ופשטותו. תכנתים משתמשים בו לשמירת קונפיגורציות, התקשורת עם API-ים ועוד.

## איך לעשות:
בשביל לעבוד עם JSON ב-Bash, נצטרך כלי כמו `jq`. הנה דוגמה פשוטה:

```Bash
echo '{"שם": "דני", "עיר": "תל אביב"}' | jq '.שם'
```

וזה יוציא:

```
"דני"
```

אפשר גם לשנות נתונים:

```Bash
echo '{"שם": "דני", "עיר": "תל אביב"}' | jq '.עיר = "ירושלים"'
```

זה יחזיר:

```json
{
  "שם": "דני",
  "עיר": "ירושלים"
}
```

## צלילה עמוקה
JSON נוצר בתחילת שנות ה-2000 כ- JavaScript Object Notation, אבל מהר מאוד הפך לעצמאי. חלק מהאלטרנטיבות כוללות XML ו-YAML, שכל אחד יש לו יתרונות וחסרונות משלו. ב-Bash, עבודה עם JSON דורשת לרוב כלים חיצוניים כמו `jq` כיוון שלא קיימת תמיכה ילידית לפרסור ויצירת JSON.

## ראו גם
- [jq Manual](https://stedolan.github.io/jq/manual/)
- [Bash Scripting Cheatsheet](https://devhints.io/bash)
- [JSON Introduction](https://www.w3schools.com/whatis/whatis_json.asp)
