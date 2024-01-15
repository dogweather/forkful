---
title:                "עבודה עם yaml"
html_title:           "Ruby: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

למה לעסוק ב YAML? לכמה מיינד תהיה זו הדרך הטובה ביותר לחסוך זמן וכסף ביצוע של פעולות מורכבות כמו אחסון נתונים והעברתם בין מכשירים ומערכות.

## איך לעסוק ב YAML

כדי להתחיל לעבוד עם YAML, יש להתקין את הספרייה המתאימה על ידי הרצת הפקודה:
```Ruby
gem install psych
```
לאחר מכן, ניתן להשתמש בסינטקס הבא כדי לקרוא ולכתוב קבצי YAML:
```Ruby
require 'yaml'

# קריאת קובץ YAML
data = YAML.load_file("example.yaml")
puts data.inspect

# כתיבת קובץ YAML
hash = {name: "עמית", age: 28, occupation: "מתכנת"}
puts YAML.dump(hash)
```
### פלט
```Ruby
{"name"=>"עמית", "age"=>28, "occupation"=>"מתכנת"}
---
:name: עמית
:age: 28
:occupation: מתכנת
```

## חקירה מקיפה

YAML היא שפת תמונת מקור מאוד מפותחת ופשוטה לשימוש. הוא מאפשר לך לאחסן ולקרוא מידע בפורמט נוח ומסודר. יתרה מזה, YAML מאפשר יצירת מבנה דטה מתואם בין תכני התמונה לפרטים הכוללים בקובץ.

רוב הפעמים, YAML משמש כקובץ הגדרות לתוכניות שונות כגון מנהלי תצורה, מערכת רישום לאתר וכו'. זהו דרך נוחה ומדויקת יותר לאחסון וניהול נתונים מאשר אנסמב"ל שמשמש לפעמים כברירת מחדל להגדרת התצורה של יישומי ווב.

למידע נוסף על YAML, ניתן לעיין במדריך הרשמי של Ruby על [העבודה עם YAML](https://ruby-doc.org/stdlib-2.7.0/libdoc/psych/rdoc/Psych.html) או בספריית YAML של ליבת המחשב [YAML](https://yaml.org/).

## ראו גם

למידע נוסף על עבודה עם Ruby, ניתן לעיין במדריך הרשמי [The Ruby Programming