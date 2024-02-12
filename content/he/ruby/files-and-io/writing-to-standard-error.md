---
title:                "כתיבה לשגיאה התקנית"
aliases: - /he/ruby/writing-to-standard-error.md
date:                  2024-02-03T19:35:01.991421-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה הסטנדרטית (stderr) ברובי מדברת על הכוונת הודעות שגיאה או ניתוח נפרד לזרם פלט נפרד, שונה מהפלט הסטנדרטי (stdout). מתכנתים עושים זאת כדי להבחין בין פלט תכנית רגיל לבין שגיאות ומידע לניפוי באגים, מה שמקל על אבחון בעיות וניתוח יומנים.

## כיצד לעשות זאת:
ספריית הסטנדרט של רובי מספקת דרך ישירה לכתיבה ל-stderr באמצעות `$stderr` או `STDERR`. אין צורך בספריות צד שלישי לפעולה בסיסית זו.

### כתיבת הודעה פשוטה ל-stderr:
```ruby
$stderr.puts "שגיאה: קובץ לא נמצא."
# או באופן שקול
STDERR.puts "שגיאה: קובץ לא נמצא."
```
פלט לדוגמא (ל-stderr):
```
שגיאה: קובץ לא נמצא.
```

### הפניית stderr לקובץ:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "נכשל בפתיחת הגדרה."
end
```
קטע קוד זה מפנה את stderr לקובץ בשם `error.log`, וכל שגיאות הכתובות שתיעשינה בהמשך יירשמו לשם עד שהתכנית תאפס את ההפניית stderr או תסתיים.

### שימוש ב-stderr עם טיפול ביוצאות מן הכלל:
```ruby
begin
  # סימולציה של פעולה שעלולה לכשל, למשל, פתיחת קובץ
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "אירעה חריגה: #{e.message}"
end
```
פלט לדוגמא (ל-stderr):
```
אירעה חריגה: לא נמצא קובץ או תיקייה @ rb_sysopen - nonexistent_file.txt
```

למרות ששיטות המובנות של רובי לכתיבה ל-stderr מספיקות לרוב היישומים, לצרכים מורכבים יותר של תיעוד שגיאות, עשוי להיות שתרצו לשקול את ספריית ה-`logger` הסטנדרטית או גמים חיצוניים כמו `Log4r`. אלו מספקות מנגנוני תיעוד ניתנים להגדרה, כולל רמות חומרה, עיצוב והיכולת לכתוב לפלטים שונים, כולל קבצים, דוא"ל ועוד.
