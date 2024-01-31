---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
simple_title:         "עבודה עם קבצי CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
CSV (ערכים מופרדים בפסיקים) היא פורמט קובץ נפוץ לנתונים טבלאיים. תכניתנים משתמשים בו כדי לייבא ולייצא נתונים בצורה נוחה וקריאה גם לאנשים וגם למחשבים.

## איך לעשות:
```Ruby
require 'csv'

# שמירת נתונים לקובץ CSV
CSV.open('example.csv', 'wb') do |csv|
  csv << ["שם", "גיל", "עיר"]
  csv << ["רועי", 30, "תל אביב"]
  csv << ["נועה", 28, "ירושלים"]
end

# קריאת נתונים מקובץ CSV
CSV.foreach('example.csv', headers: true) do |row|
  puts "#{row['שם']} בן/בת #{row['גיל']}, מ#{row['עיר']}"
end
```

פלט דוגמה:
```
רועי בן/בת 30, מתל אביב
נועה בן/בת 28, מירושלים
```

## עיון עמוק:
CSV הוא פורמט שמקורו בשנות ה-70 ומשמש כתקן עד היום. ישנם אלטרנטיבות כמו קובץ JSON או XML, אך CSV נשאר פופולרי בשל הפשטות שלו. הספרייה המובנית בRuby לעבודה עם CSV מאפשרת מניפולציית נתונים בקלות ובאופן יעיל.

## ראה גם:
- [Ruby CSV Parsing and Writing (מאמר על ניתוח וכתיבה של CSV בRuby)](https://www.rubyguides.com/2018/10/parse-csv-ruby/)
- [Stack Overflow: Questions tagged 'ruby' and 'csv'](https://stackoverflow.com/questions/tagged/ruby+csv)
