---
title:    "Swift: כתיבה לאירעות סטנדרטיות"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# למה

לכתוב לטעות המכוון עם Swift תחילה נראה כמו רעיון מוזר. אך ישנם מקרים בהם מומלץ לרשום לטעות שגיאות ומידע נוסף לכדי עזרה בזמן העלילה.

# כיצד

על מנת לכתוב לטעות המכוון בלישה של הצליחה, ניתן להשתמש בפונקציה `print()` עם הפרמטר `to:` לשלוח את המסר לכתוב במכוון זה. לדוגמה:

```Swift
print("ישנה שגיאה בקוד", to: &stdout)
```

כאשר `stdout` הוא המכוון של השגיאה הרצויה להופיע בו. ניתן להשתמש גם במכוון `stderr`, כך הודעות השגיאה יופיעו בכתובן הקבוע ולא בקובץ פלט. ניתן גם להשתמש במשתנה `FileHandle.standardError` כדי לשלוח את המסר למכוון בקובץ.

# העמקת הנושא

כפי שניתן לראות בדוגמאות, כתיבה לטעות המכוון עם Swift היא דבר פשוט ובסיסי. המכוונים משתמשים בפרמטר `inout` כדי לעדכן את המייצגים של המכוונים. ניתן גם להשתמש בפונקציות מובנות, כגון `FileHandle.standardError`, כדי לכתוב למכוון בקובץ במכוון שלהם.

# ראה גם

- [עורך טקסט של שפת Swift עבור הישרדור ואולם המכוון](https://www.coprub.com/b/editor-de-texto-for-swift-depuracao-por-console-e-mayhap)
- [רשימת פקודות ב-PHP לכתיבת למכוון רשת](https://www.php.net/manual/en/function.fwrite.php)
- [מדריך מפורט לכתיבת לטעות המכוון בשפת Java](https://www.javatpoint.com/how-to-write-to-file-in-java)