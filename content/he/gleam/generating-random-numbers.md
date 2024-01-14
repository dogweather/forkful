---
title:                "Gleam: יצירת מספרים אקראיים"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# למה

הכיוון המושג והמגוון של בעיה מחייב לבצע חישובים בצורה אקראית. בעזרת תכנות עם Gleam נוכל לייצר את המספרים האקראיים הדרושים בקלות ולפתור את הבעיה בצורה מהירה ויעילה.

# איך לעשות זאת

הנה דוגמה לקוד לייצור מספרים אקראיים ב-Gleam:

```Gleam
import gleam/random

// לייצר מספרים אקראיים בין 1 ל-10
let random_number = random.int(1, 10)
```

כאן ניתן לראות שהשתמשנו בפונקציית random מתוך המודול gleam/random כדי לייצר מספרים אקראיים. הפרמטרים הראשונים הם גבולות הטווח שבתוכו ייצר המספר האקראי.

כעת, נדפיס את המספר האקראי שיצרנו כדי לראות את הפלט הסופי:

```Gleam
import gleam/random

let random_number = random.int(1, 10)
// דוגמה לחיבור של מחרוזת יחד עם המספר האקראי
random_number |> IO.format("המספר האקראי הוא: {}", [IO.integer])
```

כאשר נריץ את הקוד שמופיע למעלה, נקבל פלט דומה לזה:

```
המספר האקראי הוא: 7
```

בכדי לייצר מספרים אקראיים בין 1 למספר הסופי המבוקש, נשתמש בפונקציית random.float כפי שמוצג בקוד למטה:

```Gleam
import gleam/random

let random_float = random.float(1, 100.0)
// דוגמה לחיבור של מחרוזת יחד עם המספר האקראי
random_float |> IO.format("המספר האקראי הוא: {}", [IO.float])
```

הפלט יהיה משהו דומה לזה:

```
המספר האקראי הוא: 65.239820654
```

# חפירה מעמיקה

ב-Gleam ישנם ספריות נוספות שמכילות פונקציות נוספות לייצור מספרים אקראיים. למשל, ספריית bitstring מכילה פ