---
title:                "עובדים עם csv"
html_title:           "Ruby: עובדים עם csv"
simple_title:         "עובדים עם csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## מה זה & למה? 
עבודה עם קבצי CSV (חיתוך ערכים מופרדים בפסיק) היא פעולה נפוצה בקוד של רובי שמאפשרת קריאה וכתיבה של נתונים בפורמט זה. תכניתנים משתמשים בקבצי CSV כדי לקרוא ולארגן מידע ממקורות שונים, כגון נתוני לקוחות או מערכת כתיבת יומן.

## איך לעשות?
לפניכם יש שתי אפשרויות בכדי לעבוד עם קבצי CSV ברובי. האפשרות הראשונה היא להשתמש בספריית CSV הכלולה כבר בתקנון של רובי. הבאת הספרייה נעשית באמצעות הפקודה `require 'csv'`. עם ספריית CSV, תוכלו לקרוא, לכתוב ולעדכן נתונים בקבצי CSV בקלות. האפשרות השנייה היא להשתמש בספרייה חיצונית כגון `FasterCSV` או `SmarterCSV` שמציעות יכולות נוספות ומתקדמות לעבודה עם קבצי CSV.

```ruby
# ניתן לעבוד עם קובץ CSV כמו כל קובץ אחר
CSV.foreach("my_file.csv") do |row|
  puts row.inspect
end
```

בנוסף, ניתן לכתוב נתונים לקובץ CSV בקלות:

```ruby
CSV.open("my_file.csv", "w") do |csv|
  csv << ["מספר לקוח", "שם מלא", "כתובת"]
  csv << [1, "דוד כהן", "רחוב ישראל 1"]
  csv << [2, "שרה לוי", "הרצל 52"]
end
```

הנתונים שמתקבלים ירשמו לקובץ בפורמט של טבלה גלויה ונוח לקריאה.

## מעמקים
בעבר, השתמשנו בתוכניות כמו Microsoft Excel כדי לקרוא ולאחסן נתונים בפורמט של קבצי CSV. אך כיום, ישנן בעיות אבטחה ותאימות עם מערכות אחרות. עם זאת, הנוחות של עבודה עם קבצי CSV באמצעות קוד חיפושי מחמיאה ויכולות המתקדמות של ספריות כמו `FasterCSV` הופכות את הכתיבה הידנית יתרון במידה רבה.

## ראו גם
- האתר הרשמי של רובי: https://www.ruby-lang.org/
- תיעוד על ספריית CSV: https://ruby-doc.org/stdlib/libdoc/csv/rdoc/CSV.html
- חבילות חיצוניות לעבודה עם קבצי CSV: https://rubygems.org/gems/faster_csv https://rubygems.org/gems/smarter_csv