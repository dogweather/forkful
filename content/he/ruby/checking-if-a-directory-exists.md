---
title:                "Ruby: בדיקה אם תיקייה קיימת"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה:
ניתן להשתמש בתכנות רובי כדי לבדוק האם תיקייה קיימת לפני ביצוע פעולות נוספות עליה. זה יכול למנוע שגיאות ולעשות את הקוד שלנו יותר אפקטיבי ומדויק.

## איך לבדוק האם תיקייה קיימת:
שתי דרכים פשוטות לבדוק האם תיקייה קיימת בעזרת תכנות רובי. 
בקוד הראשון, אנו משתמשים בפונקציה File.exist? שבודקת אם התיקייה קיימת:
```Ruby
if File.exist?("my_directory")
  puts "התיקייה קיימת"
else
  puts "התיקייה לא קיימת"
end
```
בקוד השני, אנו משתמשים בפונקציה Dir.exist? שבודקת האם התיקייה קיימת:
```Ruby
if Dir.exist?("my_directory")
  puts "התיקייה קיימת"
else
  puts "התיקייה לא קיימת"
end
```

## מסתכלים בעומק:
שתי הפונקציות File.exist? ו-Dir.exist? מבוססות על הפונקציה File.ftype שמחזירה את סוג הקובץ שהיא מקבלת כפרמטר. במקרה של תיקייה, הפונקציות משתמשות בסוג הקובץ `directory` כדi לבדוק האם התיקייה קיימת. בנוסף, ניתן להשתמש בפונקציות אלו לבדוק אם קובץ מסוים קיים, על ידי הכנסת כתובת הקובץ כפרמטר.
תרגום של הפונקציות:
- File.exist? = `קיים_קובץ?`
- Dir.exist? = `קיימת_תיקייה?`
- File.ftype = `סוג_קובץ`

## ראו גם:
- [רמזים לתכנות רובי](https://www.rubyguides.com/ruby-tutorial/)
- [מדריך לפונקציות בתכנות רובי](https://ruby-doc.org/core-2.7.2/)
- [הסברים מפורטים על מבנה תיקיות וקבצים במערכת קבצים של וינדוס](https://docs.microsoft.com/en-us/windows/win32/fileio/file-and-directory-names)