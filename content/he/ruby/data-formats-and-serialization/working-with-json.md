---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:28.534691-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Ruby, \u05E2\u05DD\
  \ \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\
  \ \u05E9\u05DC\u05D5, \u05DE\u05E1\u05E4\u05E7\u05EA \u05D3\u05E8\u05DB\u05D9\u05DD\
  \ \u05D7\u05DC\u05E7\u05D5\u05EA \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\
  \u05E6\u05D9\u05E8\u05D4 \u05E9\u05DC JSON. \u05D4\u05DE\u05D5\u05D3\u05D5\u05DC\
  \ \u05D4\u05E2\u05D9\u05E7\u05E8\u05D9 \u05DC\u05E4\u05E2\u05D5\u05DC\u05D5\u05EA\
  \ \u05D0\u05DC\u05D5 \u05D4\u05D5\u05D0 `json`, \u05E9\u05E0\u05D9\u05EA\u05DF \u05DC\
  \u05E9\u05DC\u05D1 \u05D1\u05E7\u05DC\u05D5\u05EA \u05D1\u05DB\u05DC \u05D0\u05E4\
  \u05DC\u05D9\u05E7\u05E6\u05D9\u05D9\u05EA Ruby."
lastmod: '2024-04-05T21:53:41.220022-06:00'
model: gpt-4-0125-preview
summary: "Ruby, \u05E2\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\
  \u05E0\u05D3\u05E8\u05D8 \u05E9\u05DC\u05D5, \u05DE\u05E1\u05E4\u05E7\u05EA \u05D3\
  \u05E8\u05DB\u05D9\u05DD \u05D7\u05DC\u05E7\u05D5\u05EA \u05DC\u05E0\u05D9\u05EA\
  \u05D5\u05D7 \u05D5\u05D9\u05E6\u05D9\u05E8\u05D4 \u05E9\u05DC JSON."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## איך לעשות:
Ruby, עם ספריית הסטנדרט שלו, מספקת דרכים חלקות לניתוח ויצירה של JSON. המודול העיקרי לפעולות אלו הוא `json`, שניתן לשלב בקלות בכל אפליקציית Ruby.

### ניתוח JSON:
כדי להמיר מחרוזת JSON למערך של Ruby, ניתן להשתמש במתודה `JSON.parse`.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# פלט: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### יצירת JSON:
באופן הפוך, כדי להמיר מערך של Ruby למחרוזת JSON, אתה משתמש במתודה `JSON.generate` או במתודה `to_json` הזמינה על אובייקטים של Ruby לאחר שהספרייה `json` נטענה.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# פלט: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### ספריות צד שלישי:
למרות שספריית הסטנדרט של Ruby מכסה את הטיפול הבסיסי בJSON, פרויקטים רבים מסתמכים על ספריות צד שלישי עבור פונקציונליות מתקדמת וביצועים. בחירה פופולרית היא `Oj` (Optimized JSON).

#### ניתוח עם Oj:
```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# פלט: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### יצירה עם Oj:
Oj גם מציעה דרך מהירה ליצירת JSON מאובייקטים של Ruby:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# פלט: {"name":"Samantha","age":35,"city":"Miami"}
```

דוגמאות אלו ממחישות את האופי הישיר של עבודה עם JSON בRuby, הופכות אותו לנגיש למשימות החל ממניפולציות נתונים פשוטות ועד לתקשורת API מורכבת.
