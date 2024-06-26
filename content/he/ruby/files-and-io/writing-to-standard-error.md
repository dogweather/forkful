---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:01.991421-07:00
description: "\u05DB\u05D9\u05E6\u05D3 \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\
  \u05EA: \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\
  \u05D8 \u05E9\u05DC \u05E8\u05D5\u05D1\u05D9 \u05DE\u05E1\u05E4\u05E7\u05EA \u05D3\
  \u05E8\u05DA \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05DB\u05EA\u05D9\u05D1\u05D4\
  \ \u05DC-stderr \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA `$stderr` \u05D0\u05D5\
  \ `STDERR`. \u05D0\u05D9\u05DF \u05E6\u05D5\u05E8\u05DA \u05D1\u05E1\u05E4\u05E8\
  \u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DC\u05E4\u05E2\
  \u05D5\u05DC\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05D6\u05D5."
lastmod: '2024-03-13T22:44:40.232125-06:00'
model: gpt-4-0125-preview
summary: "\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\
  \u05D8 \u05E9\u05DC \u05E8\u05D5\u05D1\u05D9 \u05DE\u05E1\u05E4\u05E7\u05EA \u05D3\
  \u05E8\u05DA \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05DB\u05EA\u05D9\u05D1\u05D4\
  \ \u05DC-stderr \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA `$stderr` \u05D0\u05D5\
  \ `STDERR`."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

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
