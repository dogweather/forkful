---
title:                "עבודה עם json"
html_title:           "Ruby: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם JSON היא תהליך שבו מתאפשר לקרוא, לכתוב ולהמיר נתונים בפורמט טקסט מקודד על פני מחשבים וגישות אינטרנט. פונקציות מובנות לתמיכה בתקווה זו בוטלו מפי מפתחי תוכנה רבים.

## איך לעבוד:

הוספת הספריה `json` תאפשר לך לקרוא, לכתוב ולהמיר בין שורות. כדי לקרוא קובץ JSON, תוכל להשתמש בפקודת `File.open` ולהוסיף את התאריך של האובייקט `JSON.load`. להמיר פורמטים, תוכל להשתמש בפונקציות `to_json` ו- `parse` כדי להמיר אובייקטים ב-Ruby ל-JSON ולהפוך.

```Ruby
require 'json'

# קריאת קובץ JSON
file = File.open(your_json_file)
data = JSON.load(file)

# כתיבת קובץ JSON
your_ruby_object.to_json

# המרת אובייקט ל-JSON
JSON.parse(your_ruby_object)
```

## להעמיק:

כיום, פופולריות שאילתאות אינטרנט כמו REST API עוקבות אחר פורמט JSON לקריאה וכתיבה של נתונים. בעבודה עם תוכנות אחרות, פרטי הקשר עלולים להשתנות בין פורמטים. עם ביצוע מעבר נמהר לביצועים והפכו לאפשר ההתקנים לקרוא ולכתוב ל- JSON שבורת פשוט עם REST API.

## ראה גם:

- [מדריכי JSON עבור Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html)
- [ספריית JSON עבור Ruby על גיטהאב](https://github.com/flori/json)
- [תיעוד עבור JSON ו-Ruby](https://docs.ruby-lang.org/en/2.6.0/json/JSON.html)