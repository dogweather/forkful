---
title:                "חיפוש והחלפת טקסט"
html_title:           "Ruby: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט הוא פעולה בה תוכנתנים נועדו למצוא ולהחליף מחרוזות טקסט בתוך קוד. זהו כלי חשוב עבור מתכנתים כיוון שהוא מאפשר להם לשנות טקסטים באופן מהיר ונוח בקוד שלהם.

## איך לעשות?

להלן כמה דוגמאות של קוד בשפת רובי שמציגות כיצד לבצע חיפוש והחלפת טקסט:

```ruby
# חיפוש התאמת מחרוזת טקסט בתוך מחרוזת אחרת
string = "שלום עולם"
match = string.match(/עולם/)
puts match[0] #=> עולם

# החלפת מחרוזת טקסט במחרוזת אחרת
string = "hello world"
new_string = string.gsub("hello", "bye")
puts new_string #=> bye world

# החלפת תבנית בתוך מחרוזת טקסט
string = "123-456-7890"
new_string = string.gsub(/(\d+)-(\d+)-(\d+)/, '\1-\3-\2')
puts new_string #=> 123-7890-456
```

## צלילת מקור

חיפוש והחלפת טקסט הוא טכניקה מאז ימי התכנות הקלאסיים. בעבר, מתכנתים היו נאלצים להשתמש בחיפוש ידני והחלפה של טקסט כדי לבצע שינויים בקוד. אך כיום, ישנם מספר כלים זמינים למתכנתים שמאפשרים חיפוש והחלפת טקסט באופן אוטומטי.

אנו ממליצים להשתמש במתודות שפת התכנות שלך כדי לבצע חיפוש והחלפת טקסט, במקום להשתמש בכלים חיצוניים כגון בשל.

## ראה גם

- [התיעוד של רובי עבור פעולות חיפוש והחלפה](https://ruby-doc.org/core-2.7.1/String.html#method-i-match)
- [פוסט על חיפוש והחלפה ברובי בבלוג של טום נונמקר](https://www.digitalocean.com/community/tutorials/how-to-use-regex-in-ruby)
- [ספרייה פופולרית לטיפול במחרוזות ברובי: Stringex](https://github.com/rsl/stringex)