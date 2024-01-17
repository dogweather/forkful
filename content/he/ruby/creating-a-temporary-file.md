---
title:                "יצירת קובץ זמני"
html_title:           "Ruby: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא ידיעה ליצירת קובץ שנמחק בסיום התוכנית. כלומר, הקובץ אינו נשאר במערכת כאשר התוכנית מסתיימת. תכנותים משתמשים ביצירת קבצים זמניים כדי לעבד נתונים מזמן פתיחת התוכנית או כדי להתמודד עם בעיות בטיפול בקבצים.

## איך לעשות:
בתוך קוד רובי ישנם מספר דרכים ליצור קבצים זמניים, בין אם באמצעות חבילת "tempfile" או באמצעות פקודת "File.open" עם הפרמטר TEMPFILE. להלן דוגמאות לשני הדרכים:

```ruby
# באמצעות חבילת "tempfile"
require "tempfile"

tempfile = Tempfile.new("mytempfile") # יצירת קובץ זמני בעזרת חבילת "tempfile"
tempfile.puts "This is a temporary file." # כתיבת תוכן לקובץ
puts tempfile.read # הדפסת תוכן הקובץ

tempfile.close # סגירת הקובץ
tempfile.unlink # מחיקת הקובץ

# באמצעות פקודת "File.open" עם הפרמטר TEMPFILE
File.open("mytempfile", "w+", tempfile: true) do |file| # יצירת קובץ זמני בעזרת פקודת "File.open"
  file.puts "This is a temporary file." # כתיבת תוכן לקובץ
  puts file.read # הדפסת תוכן הקובץ
end # סגירת הקובץ ומחיקתו אוטומטית

```

## חקירה מעמיקה:
יצירת קבצים זמניים היא טכניקה נפוצה בתחום התכנות, והיא משמשת לצרכים שונים, כגון כתיבת נתונים לקובץ זמני כדי להמשיך לעבוד עם הנתונים במהלך התוכנית או כדי לפתור בעיות קשורות לטיפול בקבצים. בנוסף, קיימים חבילות נוספות בעלות פונקציות נלוות ליצירת קבצים זמניים.

## לצפות גם:
* דוגמה ליצירת קובץ זמני בעזרת gem pry-doc: https://github.com/pry/pry-doc
* טכניקה נוספת ליצירת קובץ זמני: https://stackoverflow.com/questions/18875982/how-to-create-temporary-file-in-ruby