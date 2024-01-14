---
title:    "PHP: יצירת קובץ זמני"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# למה
יצירת קובץ זמני יכול להיות רעיון טוב במקרים כמו צורך ליצור קובץ לצורך עיבוד זמני של מידע או בשימוש בתחבירים שונים של קובץ במהלך תהליך תכנות או טסטים.

# כיצד להשתמש
כדי ליצור קובץ זמני ב-PHP ניתן להשתמש בפונקציה השולחת לך ישירות לקובץ זמני נוכחי ביחס לסקריפט PHP. לדוגמא:

```PHP
$tempfile = tmpfile(); // יצירת קובץ זמני
echo fwrite($tempfile, "טקסט כלשהו"); // כתיבת טקסט לקובץ זמני
rewind($tempfile); // מחיקת קורא וכתיבה לתחילת הקובץ
echo fread($tempfile, filesize($tempfile)); // קריאה מהקובץ והדפסת התוכן
fclose($tempfile); // סגירת הקובץ הזמני
```

תוצאה יכולה להיות דומה לזו:

> טקסט כלשהו

# חקירה מעמיקה
ליצור קובץ זמני ב-PHP יכול להיות מסע מעניין כמו נושאים נוספים בתוך תכנות כמו הצגת צורות, זמנים כמו צרכי זיכרון וסיומות בעזרת פונקציות של פייפ שי.

# ראה גם
- [דוגמאות שימוש בפוקנציות קבצים בפייפ שי בקישור זה](https://www.php.net/manual/en/function.fopen.php)
- [התיעוד של הפונקציות הקיימות של קובץ PHP בגרסת 7.4](https://www.php.net/manual/en/ref.filesystem.php)
- [נושאים נוספים על יצירת קובץ זמני במדריך זה](https://www.geeksforgeeks.org/php-temporary-files/)