---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:17.595124-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
גילוי מספרים אקראיים הוא תהליך שבו מחוללים מספרים באופן שאפשר לצפות מראש. תוכניתנים עושים את זה לשלל יישומים: מבדיקות תוכנה ומשחקים ועד קריפטוגרפיה והגרלות.

## איך לעשות:
Go זה שפה מעולה לגילוי מספרים אקראיים. כאן קוד לדוגמה שתופס כמה מצבים:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// איתחול המחולל האקראי
	rand.Seed(time.Now().UnixNano())

	// גילוי מספר אקראי בין 0 ל-100
	randomNum := rand.Intn(101)
	fmt.Printf("מספר אקראי: %d\n", randomNum)
}
```

כאשר תריץ קוד זה, תקבל מספר אקראי חדש בכל פעם, כמו:
```
מספר אקראי: 47
```

## צלילה עמוקה
מה שאנחנו קוראים "אקראי" במחשבים זה לא באמת אקראי - זה תוצר של אלגוריתם דטרמיניסטי שנקרא "מחולל פסבדו-אקראי" (PRNG). הם תלויים בערך התחלתי - "זרע", שאם תידע תוכל לחזות את התוצאה.

לפני שהסטנדרט הנוכחי של Go צץ, היו שלל אלגוריתמים לגילוי אקראי, כמו LCG (Linear Congruential Generator) וMT19937 (Mersenne Twister). אבל Go הולך עם מערכת פשוטה יותר ויעילה.

## ראה גם
- מסמכי ה-Go Standard Library ל`math/rand`: https://pkg.go.dev/math/rand
- ויקיפדיה על מחוללים פסבדו-אקראיים: https://he.wikipedia.org/wiki/מחולל_מספרים_הסתברותי
- תיעוד נוסף על קריפטוגרפיה ב-Go (עבור גילוי מספרים אקראיים בטוחים יותר): https://golang.org/pkg/crypto/rand/