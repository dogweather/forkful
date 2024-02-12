---
title:                "עובדים עם CSV"
aliases: - /he/ruby/working-with-csv.md
date:                  2024-02-03T19:21:45.249152-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV ברובי מספקת גישה ישירה לטפל בנתונים טבלאיים. מתכנתים לעיתים קרובות מתעסקים בפעולה זו לצורכי ניתוח נתונים, חילוץ, המרה, ואחסון, הופכת אותה לכישור קריטי למשימות הכוללות מניפולציה או ניתוח נתונים.

## איך ל:

רובי כוללת כברירת מחדל את הספרייה CSV, אשר מפשטת את הקריאה מתוך והכתיבה אל קבצי CSV. הנה איך אתה יכול לנצל את זה למשימות נפוצות:

### קריאת קובץ CSV
לקרוא מקובץ CSV, תחילה דרושה הספרייה CSV. לאחר מכן, ניתן לעבור על השורות או לקרוא אותן לתוך מערך.

```ruby
require 'csv'

# קריאת כל שורה כמערך
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# הפלט לכל שורה עשוי להיראות כך: ["data1", "data2", "data3"]
```

### כתיבה ל-CSV
כתיבה לקובץ CSV גם היא פשוטה. ניתן להוסיף לקובץ קיים או ליצור קובץ חדש לכתיבה.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# זה יוצר או דורס את 'output.csv' עם הכותרות והערכים המצוינים.
```

### ניתוח מחרוזת CSV
לפעמים יש צורך לנתח נתוני CSV ישירות מתוך מחרוזת. הנה איך:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# הפלט הצפוי:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### שימוש ב-SmarterCSV
למשימות CSV מורכבות יותר, הגולם `SmarterCSV` יכול להיות כלי שימושי. תחילה, התקן את הגולם:

```shell
gem install smarter_csv
```

לאחר מכן, אתה יכול להשתמש בו לטפל בקבצים גדולים או לבצע ניתוח ומניפולציה מתוחכמים יותר:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# זה יקרא 'large_data.csv' ויציג כל שורה כהאש בהתבסס על הכותרות.
```

לסיכום, הספרייה המובנית CSV של רובי, לצד גולמים צד שלישי כמו `SmarterCSV`, מספקת תמיכה עקבית לטיפול בנתוני CSV, מאפשרת את ביצוע משימות עיבוד ומניפולציה יעילות של נתונים.
