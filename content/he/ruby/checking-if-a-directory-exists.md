---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- he/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:31.511629-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספריה קיימת ב-Ruby מאפשרת למתכנתים לאמת את קיומה של ספריה לפני ביצוע פעולות כמו קריאת קבצים או יצירת ספריות חדשות. זה קריטי למניעת שגיאות בטיפול בקבצים ולהבטחת האמינות של מניפולציות במערכת קבצים.

## איך לעשות:
ספריית הסטנדרט של Ruby מספקת שיטות ישירות לבדיקת קיום ספריה. הנה איך עושים זאת ב-Ruby טהור, בלי צורך בספריות צד שלישי:

```ruby
require 'fileutils'

# בדוק אם ספריה קיימת
if Dir.exist?('/path/to/directory')
  puts 'הספריה קיימת.'
else
  puts 'הספריה אינה קיימת.'
end
```
דוגמא לפלט:
```
הספריה קיימת.
```
או:
```
הספריה אינה קיימת.
```

בנוסף לשימוש ב-`Dir.exist?`, ניתן גם להשתמש בשיטה `File.directory?` אשר מחזירה `true` אם הנתיב הנתון הוא ספריה:

```ruby
if File.directory?('/path/to/directory')
  puts 'הספריה קיימת.'
else
  puts 'הספריה אינה קיימת.'
end
```
גם `Dir.exist?` וגם `File.directory?` הם חלק מספריית הסטנדרט של Ruby ואינם דורשים שימוש ב-gems חיצוניים, הופכים אותם לאפשרויות נוחות ויעילות לבדיקות ספריות.
