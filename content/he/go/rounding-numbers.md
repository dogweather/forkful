---
title:                "עיגול מספרים"
date:                  2024-01-26T03:45:52.423776-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים משמעותו תיקון של מספר לקראת השלם הקרוב אליו או למקום עשרוני מסוים. זה נעשה כדי לפשט ערכים, להפוך אותם לקריאים יותר או להתאים אותם למגבלות מסוימות, כמו כאשר עובדים עם מטבעות.

## איך לעשות:
חבילת ה-`math` בגו היא החבר שלך לעיגול. השתמש ב-`math.Round`, `math.Floor`, ו-`math.Ceil` לפשטות:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // עגל למספר שלם הקרוב
	fmt.Println("Floor:", math.Floor(number)) // עגל כלפי מטה
	fmt.Println("Ceil: ", math.Ceil(number))  // עגל כלפי מעלה
}
```

תוצאת דוגמה:
```
Round: 3
Floor: 3
Ceil: 4
```

למקומות עשרוניים ספציפיים, הכפל, עגל ולאחר מכן חלק:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("מעוגל לשני מקומות עשרוניים:", roundToDecimalPlace(number, 2))
}
```

תוצאת דוגמה:
```
מעוגל לשני מקומות עשרוניים: 3.14
```

## צלילה עמוקה
עיגול מספרים לא חדש - הוא נמצא כבר מאז ימי המתמטיקה העתיקה, תמיד במטרה לפשט. ה-`math.Round` בגו משתמשת ב[עיגול הבנקאים](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even), משמעות הדבר היא ש-0.5 מעוגל למספר הזוגי הקרוב, מה שמפחית נטייה שעלולה להשפיע על סכומים.

מספרים בצפיפות נקודה יכולים להיות מסובכים בשל הייצוג הבינארי שלהם, אשר עלול לא לייצג בדיוק את כל העשרוניים.  גישת גו, למרות זאת, שומרת על התנהגות צפויה ברוב המקרים.

קיימות שיטות עיגול אחרות, כגון "עיגול חצי כלפי מעלה" או "עיגול חצי הרחק מאפס", אך ספריית הסטנדרט של גו היא מה שזמין באופן מיידי. לצרכים מורכבים יותר, ייתכן שתצטרך להשתמש בספרייה של צד שלישי או ליצור פתרון משלך.

## ראה גם
- חבילת ה-`math` של גו: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- הסטנדרט IEEE 754 לחישוב נקודה צפה (בסיס גו לטיפול בנקודות צפה): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- הבנת נקודה צפה: ["מה כל מדען מחשב צריך לדעת על חישוב נקודה צפה"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)