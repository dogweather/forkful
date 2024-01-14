---
title:                "Go: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

### למה
מיליוני פעמים ביום, מתכנתים נקראים להגרלות. זה יכול להיות ראוי בקוד לצורך הגרלת מספר רנדומלי, לטוב או לרע. במאמר זה, נלמד כיצד ליצור מספרים אקראיים בשפת תכנות Go ולהבין כיצד זה עובד.

### כיצד לעשות זאת
הרצת תכנית קצר ופשוט בשפת תכנות Go כדי ליצור מספרים רנדומליים משמר בין 0 ו-100:

```go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    fmt.Println(rand.Intn(101))
}
```

מספר הופק במקרה הזוכה להיות 23 בפעם הראשונה שבה הקוד הוטרם הופעל.

### העמקה
יצירת מספרים רנדומליים נעשית על ידי שימוש באלגוריתם בסיסי הנקרא "גנרטור רנדומלי לינארי בקמ" (Linear Congruential Generator או LCG). האלגוריתם משתמש בבנאי חשבון כדי ליצור סדרה של מספרים רנדומליים עם תכונות מבניות כגון נפח הוספה, מכפלה, מודולו וקרברונג. כדי ללמוד עוד על השיטה החשבונית ליצירת מספרים רנדומליים בשפת Go, ניתן לעיין במאמר זה בויקיפדיה: https://en.wikipedia.org/wiki/Linear_congruential_generator

### ראה גם
- מדריך לשפת תכנות Go: https://tour.golang.org/welcome/1
- גיטהאב של שפת תכנות Go: https://github.com/golang/go
- דף הבית של Go: https://golang.org/