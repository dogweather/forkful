---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:58:27.709785-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה בדיקה אם דירקטוריה קיימת ולמה זה נדרש? בפשטות, זוהי הליך שבו התכנית שלנו מוודאת אם דירקטוריה (תיקייה) קיימת במערכת הקבצים. תכניתנים צריכים לעשות זאת לפני שהם יוצרים, מוחקים, או משנים קבצים בתוך דירקטוריה, כדי למנוע שגיאות ולהבטיח את תקינות התהליך.

## How to:
קוד Ruby פשוט שמראה איך לבדוק אם דירקטוריה קיימת:

```ruby
require 'fileutils'

# שיטה לבדוק אם דירקטוריה קיימת
def directory_exists?(directory_path)
  File.directory?(directory_path)
end

# דוגמה לשימוש
if directory_exists?('/path/to/my/directory')
  puts 'הדירקטוריה קיימת!'
else
  puts 'הדירקטוריה לא נמצאה.'
end
```

תוצאה אפשרית:
```
הדירקטוריה קיימת!
```
או אם היא לא קיימת, תראה:
```
הדירקטוריה לא נמצאה.
```

## Deep Dive:
בעבר, כדי לבדוק אם דירקטוריה קיימת ב-Ruby, מתכנתים לעיתים ישתמשו בשיטות ישנות יותר כמו `Dir.exist?` או הספרייה `FileUtils`. כיום, `File.directory?` היא השיטה הנפוצה והמומלצת. היא מחזירה `true` או `false` בהתאם לקיום הדירקטוריה.

אם נרצה לדעת לא רק אם הדירקטוריה קיימת אלא גם לוודא שיש לנו הרשאות כתיבה או קריאה, נוכל להשתמש ב:

```ruby
File.writable?(directory_path) # בדיקת הרשאת כתיבה
File.readable?(directory_path) # בדיקת הרשאת קריאה
```

לפעולות אלו השפעות על תכנונית האבטחה של התוכנה, ויש להבין אותן היטב לפני יישום.

## See Also:
* [Ruby's File.directory? documentation](https://ruby-doc.org/core-2.7.0/File.html#method-c-directory-3F)
* [Ruby's FileUtils module documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/fileutils/rdoc/FileUtils.html)
* [Stack Overflow discussions on file and directory operations in Ruby](https://stackoverflow.com/questions/tagged/ruby+file+directory)
