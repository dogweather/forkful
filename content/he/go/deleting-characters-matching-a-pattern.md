---
title:                "מחיקת תווים התואמים לתבנית"
date:                  2024-01-20T17:42:22.646904-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים לפי דפוס היא תהליך בו מסירים סדרה של תווים ממחרוזת שמתאימים לקריטריונים מוגדרים. תכנותים עושים זאת כדי לנקות נתונים, לפשט עיבוד טקסט או להתאים אותו לדרישות מסוימות.

## איך לעשות:
כדי למחוק תווים לפי דפוס ב-Go, אתה יכול להשתמש בפקודת `Replace` של הפקג' `strings` או בביטויים רגולריים עם הפקג' `regexp`. נתחיל בדוגמה פשוטה עם `strings.Replace`.

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "היי, מה קורה? יום סבבה, נכון?"
	clean := strings.Replace(original, "סבבה", "", -1)
	fmt.Println(clean) // היי, מה קורה? יום , נכון?
}
```

ועכשיו דוגמה עם `regexp`.

```go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	original := "היי, מה קורה? 1234 יום נפלא!"
	pattern := "[0-9]+"
	re := regexp.MustCompile(pattern)
	clean := re.ReplaceAllString(original, "")
	fmt.Println(clean) // היי, מה קורה?  יום נפלא!
}
```

## צלילה עמוקה:
החלפת תווים זה משהו שאנחנו עושים מאז הימים הראשונים של עיבוד טקסט במחשבים. מעבר לפונקציות הסטנדרטיות למחיקת תווים, ביטויים רגולריים הם כלי חזק מאוד שמאפשר לך לבצע תואמות והחלפות מורכבות.

הביטוי הרגולרי שהשתמשנו בו פירושו: "כל רצף תווים שהם מספרים". ל־`regexp.MustCompile` יש עלות בזמן ריצה בפעם הראשונה כי הוא קומפל את הביטוי הרגולרי, אבל אחר כך הוא מהיר.

אם צריך לבצע החלפה פשוטה ללא דפוסים מורכבים, מומלץ להשתמש בפקודות `strings` כמו `Replace`, כי הן יהיו יעילות יותר. השימוש בביטויים רגולריים כדאי רק עבור התאמות דפוסים מורכבים יותר.

## ראה גם:
- מסמכי Go של הפקג' `strings`: https://pkg.go.dev/strings
- מסמכי Go של הפקג' `regexp`: https://pkg.go.dev/regexp
- סקירה על ביטויים רגולריים: https://www.regular-expressions.info/
- מדריך על עבודה עם מחרוזות ב-Go: https://gobyexample.com/strings
