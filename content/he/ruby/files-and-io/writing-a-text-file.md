---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:49.556713-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8 \u05D1\u05E8\u05D5\u05D1\u05D9 \u05D4\u05D9\u05D0 \u05E4\u05E2\
  \u05D5\u05DC\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05D4\u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05DC\u05DA \u05DC\u05D0\u05D7\u05E1\u05DF \u05E4\u05DC\u05D8\
  \ \u05D5\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05E7\
  \u05D1\u05D5\u05E2, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\u05D4\u05D9\u05D5\u05EA \u05E0\u05D2\u05D9\
  \u05E9\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D4\u05EA\u05E2\u05D3\u05DB\u05DF \u05DE\
  \u05D0\u05D5\u05D7\u05E8 \u05D9\u05D5\u05EA\u05E8. \u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:40.235297-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1\u05E8\u05D5\u05D1\u05D9 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\
  \u05DC\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05D4\u05DE\u05D0\u05E4\u05E9\
  \u05E8\u05EA \u05DC\u05DA \u05DC\u05D0\u05D7\u05E1\u05DF \u05E4\u05DC\u05D8 \u05D5\
  \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05E7\u05D1\
  \u05D5\u05E2, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DC\u05D4\u05D9\u05D5\u05EA \u05E0\u05D2\u05D9\u05E9\
  \u05D9\u05DD \u05D0\u05D5 \u05DC\u05D4\u05EA\u05E2\u05D3\u05DB\u05DF \u05DE\u05D0\
  \u05D5\u05D7\u05E8 \u05D9\u05D5\u05EA\u05E8. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## מה ולמה?
כתיבה לקובץ טקסט ברובי היא פעולה יסודית המאפשרת לך לאחסן פלט ונתונים באופן קבוע, מה שמאפשר לנתונים להיות נגישים או להתעדכן מאוחר יותר. תכנתים לעיתים קרובות מבצעים משימה זו מסיבות כמו לוגינג, שמירת הגדרות, או ייצוא נתונים בפורמט קריא לאדם.

## איך לעשות זאת:
רובי מקלה על פעולות קובץ. לכתיבה לקובץ, ניתן להשתמש במחלקה המובנית `File` של רובי. הדוגמה הבאה מדגימה איך לפתוח קובץ לכתיבה (במצב `"w"`) ולהוספה (במצב `"a"`), ואז לכתוב מחרוזת לתוכו, ולוודא שהקובץ נסגר לאחר מכן:

```ruby
# כתיבת תוכן חדש לקובץ, עם דריסת התוכן הקיים
File.open("example.txt", "w") do |file|
  file.puts "שלום, רובי!"
end

# הוספת תוכן לסוף הקובץ
File.open("example.txt", "a") do |file|
  file.puts "מוסיף שורה נוספת."
end
```
לאחר הרצת שני הקטעי הקוד, תוכן `example.txt` יהיה:
```
שלום, רובי!
מוסיף שורה נוספת.
```

### שימוש בספרייה חיצונית: FileUtils
לפעולות קובץ מורכבות יותר, ספריית הסטנדרט של רובי `FileUtils` יכולה להועיל, אף על פי שלכתיבת קבצים בסיסית, שיטות ה`File` הסטנדרטיות מספיקות. עם זאת, אם ברצונך להעתיק, להזיז, להסיר, או לבצע פעולות אחרות במערכת הקבצים במקביל לכתיבת קובץ, `FileUtils` שווה בדיקה.

דוגמה לשימוש ב`FileUtils` ליצירת תיקייה ולאחר מכן כתיבה לקובץ בתוך התיקייה הזו:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "רישום יומן: #{Time.now}"
end
```

זה מדגים יצירת תיקייה חדשה `logs` אם היא עדיין לא קיימת, וכתיבה לקובץ חדש `today.log` בתוכה, מציג שני פעולות על מניפולציה של תיקיות וקבצים ללא כתיבה ישירה עם FileUtils, אך בהפעלת יכולת ההתמודדות עם תיקיות שלה.
