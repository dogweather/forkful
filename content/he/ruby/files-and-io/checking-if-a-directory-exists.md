---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:31.511629-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Ruby \u05DE\u05D0\u05E4\u05E9\u05E8\
  \u05EA \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05D0\u05DE\u05EA\
  \ \u05D0\u05EA \u05E7\u05D9\u05D5\u05DE\u05D4 \u05E9\u05DC \u05E1\u05E4\u05E8\u05D9\
  \u05D4 \u05DC\u05E4\u05E0\u05D9 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\
  \u05DC\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D1\
  \u05E6\u05D9\u05DD \u05D0\u05D5 \u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\u05E4\u05E8\
  \u05D9\u05D5\u05EA \u05D7\u05D3\u05E9\u05D5\u05EA. \u05D6\u05D4 \u05E7\u05E8\u05D9\
  \u05D8\u05D9 \u05DC\u05DE\u05E0\u05D9\u05E2\u05EA \u05E9\u05D2\u05D9\u05D0\u05D5\
  \u05EA \u05D1\u05D8\u05D9\u05E4\u05D5\u05DC\u2026"
lastmod: '2024-03-13T22:44:40.228699-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D4\
  \ \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Ruby \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \ \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05D0\u05DE\u05EA \u05D0\
  \u05EA \u05E7\u05D9\u05D5\u05DE\u05D4 \u05E9\u05DC \u05E1\u05E4\u05E8\u05D9\u05D4\
  \ \u05DC\u05E4\u05E0\u05D9 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\
  \u05D5\u05EA \u05DB\u05DE\u05D5 \u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D1\u05E6\
  \u05D9\u05DD \u05D0\u05D5 \u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\u05E4\u05E8\u05D9\
  \u05D5\u05EA \u05D7\u05D3\u05E9\u05D5\u05EA. \u05D6\u05D4 \u05E7\u05E8\u05D9\u05D8\
  \u05D9 \u05DC\u05DE\u05E0\u05D9\u05E2\u05EA \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D1\u05D8\u05D9\u05E4\u05D5\u05DC\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
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
