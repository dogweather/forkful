---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא התהליך שבו אנחנו מקבלים חלק מהמחרוזת, על ידי ציון של התחלה וסוף ממנו אנו רוצים לשלוף. מתכנתים משתמשים בכך לחלק נתונים ממחרוזות ארוכות, או לראות תוכן מסוים במחרוזת.

## איך:
הקוד הבא מראה איך לחלץ תת-מחרוזת ממחרוזת נתונה ב־Go:

```Go
package main
import "fmt"

func main() {
	str := "שָׁלוֹם, עוֹלָם!"
	substr := str[0:5]
	fmt.Println(substr)
}
```
הפלט של הקוד הזה הוא:

```
"שָׁלוֹם"
```

## צלילה עמוקה:
בתחילה, שפות תכנות מוקדמות כמו C ו-Pascal שימשו ממד אנטרופליה די מסובך לחילוץ תת-מחרוזות. עם החידוש בשפות תכנות מודרניות, אנו מתקדמים לשימוש בכניסה של O(1) למידע במחרוזת. מבחינת חלופות, שפות אחרות כמו Python ו-JavaScript מציעות גם דרך נוחה לביצוע חילוץ תת-מחרוזות. הפרטים של איך Go מממשה את זה, הם מעוניינים, ומשתמשים ביכולת של Go לראות "מאחורי הקלעים" של מחרוזות.

## ראה גם:
(1) התיעוד הרשמי של Go למחרוזות: https://golang.org/pkg/strings/ 
(2) מדריך Go מקוצר למחרוזות: https://www.tutorialspoint.com/go/go_strings.htm 
(3) הקורס המקוון החופשי לGo: https://www.learn-golang.org/