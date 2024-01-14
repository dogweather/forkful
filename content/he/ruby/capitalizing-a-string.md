---
title:    "Ruby: שינוי רישות במחרוזת - Capitalizing a string"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# למה
ישנן מצבים רבים בהתכנות שבהם יש צורך להפוך את האותיות של מחרוזות לגדולות. כדוגמת, כאשר אנו רוצים להדפיס מחרוזת עם אותיות גדולות או כאשר אנו מעבדים עם רשימות של מחרוזות ורוצים שכל האיברים יתחילו באות גדולה.

# כיצד לבצע זאת
ניתן להשתמש בפונקציית `capitalize` כדי להפוך את האות הראשונה של מחרוזת לגדולה:

```Ruby
"hello world".capitalize
# הפלט: "Hello world"
```

אם נרצה להפוך את כל האותיות לגדולות, ניתן להשתמש בפונקציית `upcase`:

```Ruby
"hello world".upcase
# הפלט: "HELLO WORLD"
```

לעתים ניתן להשתמש גם בפונקציות `split` ו־`join` כדי להפוך את כל האותיות לגדולות רק עבור מספר מחרוזות ברשימה:

```Ruby
["hello", "goodbye"].map { |string| string.upcase }
# הפלט: ["HELLO", "GOODBYE"]
```

# חקירה מעמיקה
מה שפתוח הנתון באמצעות הפונקציות `capitalize` ו־`upcase` הוא שמן מחרוזת חדשה שההתחלה הראשונה שלה מיוחדת בפני עצמה. ניתן ללמוד עוד על איך מפענחים פונקציות משלבות אותיות בפסקל, באמצעות הכתבה המפורטת יותר של עקרון ה־"גלישה" ("Ladder" conversions) [כאן](https://stackoverflow.com/questions/224156/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript).

# ראו גם
- [רשימת פונקציות משלבות אותיות בפסקל באתר Forum4JExec](http://forum4executives.com/index.php?threads/%D7%A4%D7%A7%D7%95%D7%93%D7%95%D7%AA-%D7%A4%D7%95%D7%A0%D7%A7%D7%A6%D7%99%D7%95%D7%AA-%D7%94%D7%9E%D7%A9%D7%9C%D7%91%D7%95%D7%AA-%D7%91%D7%A4%D7%A1%D7%A7%D7%9C-by-forum4executives.50967/#post-53460)
- [מאמר על פ