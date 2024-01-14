---
title:                "Ruby: מחיקת תווים התואמים דפוס"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# למה:

מחיקת תווים המתאימים לתבנית מהווה כלי חזק בעולם התכנות של Ruby. יעילותו הגבוהה וקלות השימוש בו נובעות מכך שמאפשרת לנו לטפל בנתונים ולעבוד איתם בצורה נתונה ומוכנה.

# איך להשתמש:

עבור נתוני תווים אותם רוצים למחוק, ניתן להשתמש בפונקציה `delete!` כדי להסיר אותם ללא תלות בתבנית מסוימת. הנה כמה דוגמאות:

```Ruby

str = "Hello, Hebrew readers!"

puts str.delete!("l") # Output: Heo, Hebrew readers!

puts str.delete!("H") # Output: ebrew readers!

puts str.delete!("!") # Output: ebrew readers

```

בדוגמאות אלו, הפונקציה `delete!` מחזירה את המחרוזת המקורית ללא התווים המתאימים לתבנית.

# חיקוי עמוק:

ניתן להשתמש בפונקציה `delete!` כדי להסיר תווים מכל סוג שהוא, בהתאם לתבנית שנקבעה על ידי המשתמש. ניתן להגדיר תבנית מגוונת כך שהפונקציה תמחק כל תו שמתאים לתבנית, ולא רק תווים יחידים.

לדוגמא, ננסה למחוק את כל המילים המופיעות באותיות גדולות מהמחרוזת הבאה:

```Ruby
str = "HeLlO, hEbReW ReAdErS!"

puts str.delete!(/[A-Z]/) # Output: l, hbrw rdrs!
```

בדוגמא זו, התבנית המעצבנת כוללת את כל האותיות הגדולות, ולכן הפונקציה מוחקת את כל האותיות הגדולות מהמחרוזת.

# ראה גם:

- תיעוד רשמי של הפונקציה `delete!` באתר המפתחים של Ruby: https://ruby-doc.org/core-2.7.0/String.html#method-i-delete-21
- הבדר על מחיקת תווים ממחרוזת באמצעות Ruby: https://www.geeksforgeeks.org/ruby-string-delete-method-with-example/
- פוסט אחד מ- medium על טכניקות שונות למחיקת תווים בשפת Ruby: https://medium.com/@aisflat439/str