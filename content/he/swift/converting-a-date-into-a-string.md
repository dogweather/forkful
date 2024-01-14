---
title:    "Swift: המרת תאריך למחרוזת"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## למה

בעולם התכנות, ניתן למצוא המון מצבים שבהם יש צורך להמיר משתנה מאחת הטיפוסים לטיפוס אחר. המעבר בין תאריך למחרוזת טקסט הוא מה נפוץ יותר ומקל עלינו לתכנת התאריכים בצורה נוחה יותר ולהציגם בצורה הרצויה למשתמשים שלנו.

## איך לעשות זאת

כדי להמיר תאריך למחרוזת טקסט בשפת סוויפט, כדאי להשתמש במחלקת "DateFormatter". בשלב הראשון, נצטרך ליצור אינסטנס של המחלקה ולהגדיר את הפורמט של המחרוזת המייצגת את התאריך שנרצה להמיר. לדוגמה:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
```

לאחר מכן, נצטרך להשתמש בפונקציית "string(from:)" כדי להמיר את התאריך למחרוזת טקסט מתאימה. לדוגמה:

```Swift
let myDate = Date()
let dateString = dateFormatter.string(from: myDate)
print(dateString) // ידפיס: 21/03/2021
```

בנוסף, אפשר להתאים את הפורמט של המחרוזת לתקופת זמן המצויה במכשיר המשתמש באמצעות הפונקצייה "setLocalizedDateFormatFromTemplate". לדוגמה:

```Swift
dateFormatter.setLocalizedDateFormatFromTemplate("MMMM dd, yyyy")
let dateString = dateFormatter.string(from: myDate)
print(dateString) // ידפיס: מרץ 21, 2021
```

## העמקה

בנוסף להמרת תאריך למחרוזת טקסט, ניתן להשתמש במחלקת "DateFormatter" על מנת להציג תאריכים בצורה מותאמת לכל לוקליזציה, כך שהתאראיכים יוצגו בצורה הרצויה עבור המשתמשים. ניתן גם להשתמש בפונקציות נוספות כמו "string(for:)" להמרת תאריכים שבהם המשתמש מוכר כבר בלוקליזציה מסוימת.

## ראה גם

- קישור למדריך של Apple על DateFormatter: https://developer.apple.com/documentation/foundation/dateformatter