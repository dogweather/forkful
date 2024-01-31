---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON (JavaScript Object Notation) הוא פורמט המונדרש לשיתוף נתונים. תוכניתנים משתמשים בו כי הוא פשוט, קריא ונתמך ברוב השפות, כולל Ruby.

## איך עושים את זה:
```Ruby
require 'json'

# ליצירת JSON מהאש
user = { name: "דוד", age: 30, city: "תל אביב" }
user_json = user.to_json
puts user_json
# => {"name":"דוד","age":30,"city":"תל אביב"}

# לקריאת JSON והמרתו לאש
user_parsed = JSON.parse(user_json)
puts user_parsed["name"] # => דוד
```

## עיון מעמיק
JSON הופק ב-2001 כמתכתב ל-XML, אז היוו JSON ו-XML תחליפים נפוצים לשיתוף נתונים. היום, JSON נחשב יותר פשוט ויעיל ולכן הוא שכיח יותר. חשוב לדעת שבעבודה עם JSON ב-Ruby יש להוריד ולדרוש את הספריה 'json' לפני השימוש.

## למידע נוסף
- [מדריך למתכנת ה-Ruby](https://www.ruby-lang.org/en/documentation/)
- [תיעוד JSON רשמי](https://www.json.org/json-en.html)
- [פרויקט json gem ב-GitHub](https://github.com/flori/json)

**שימו לב** שקישורים אלו כוללים מידע באנגלית.
