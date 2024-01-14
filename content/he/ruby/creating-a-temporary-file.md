---
title:    "Ruby: יצירת קובץ זמני"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה
יצירת קובץ זמני היא כלי חיוני בתכנות רובי שמטרתו ליצור קובץ זמני שימושי במשך אותו הפעולה. זה משמש לא רק לכתיבה וקריאה של נתונים מתוך קובץ, אלא גם כדי לבצע פעולות כמו יצירת קבצים, עריכת קבצים ומחיקתם.

## איך לעשות זאת
הביצוע של יצירת קובץ זמני ברובי הוא פשוט ונוח, והוא מתבצע בעזרת הפעולה `Tempfile.new` והמחלקה Tempfile. ניתן לבצע על הקובץ הזמני כל פעולה שאפשר לבצע על קובץ רגיל, ולאחר מכן יש למחוק אותו באמצעות הפעולה `delete`.

```Ruby
tempfile = Tempfile.new('example.txt') 
tempfile << "זוהי מחרוזת נכתבת לקובץ הזמני"
tempfile.rewind
puts tempfile.read #=> "זוהי מחרוזת נכתבת לקובץ הזמני"
tempfile.close
tempfile.unlink #=> הקובץ הזמני נמחק
```

## חקר עמוק
כאשר משתמשים בפעולה `Tempfile.new`, הקובץ הזמני יוצר בתיקיית הפעילות. באופן כללי, הנתיב לתיקיית הפעילות היא כזו: `Dir.tmpdir` אבל ניתן לבדוק באמצעות `Tempfile.open` איפה הקובץ הזמני שלך נוצר בפועל.

בנוסף, ניתן להגדיר את הקיבוץ הזמני כפרט של מחלקת Tempfile תחת פרמטר `:tmpdir`. כך תוכלו להגדיר תיקיית קובץ זמנה ספציפית ולתת שם לקובץ הזמני שלכם.

מתוך מחלקת Tempfile, ניתן גם למצוא שדה `@tmpfile` המכיל את הקובץ הזמני עצמו.

```Ruby
tmpdir = File.join(__dir__, 'tmp') #=> יצירת התיקייה tmp באותו התיקייה כמו קובץ הראשי
tempfile = Tempfile.new('example.txt', tmpdir)
puts tempfile.path #=> "/home/user/your_project/tmp/example.txt"
puts