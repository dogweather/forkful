---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

מציאת אורך המחרוזת היא פונקציה שמחזירה את מספר התווים במחרוזת. זה נחוץ לעיתים כדי לבצע בדיקות חוקיות ולמנות מידע. 

## כיצד:

בלשון Go, אנחנו משתמשים בפקודה `len` כדי למצוא את אורך מחרוזת. נסו את זה:

```Go
package main
import "fmt"

func main() {
    str := "שלום, עולם"
    fmt.Println(len(str))
}
```

הפונקציה len מחזירה את אורך המחרוזת בבתים. במחרוזת "שלום, עולם" יש 11 בתים, כי כל תו קיים בעזרת 3 בתים.

## צלילה עמוקה:

הפונקציה len הוצגה לראשונה בגרסה הראשונה של שפת Go ומאז היא נמצאת בשימוש נרחב. בשפות תכנות אחרות, מקבילים ממשיים כמו strlen ב-C ו length ב-Java.

אטריביבטים נוספים שאפשר להשתמש בהם הם `RuneCountInString` מהספרייה `unicode/utf8` שמחזיר את מספר התווים במחרוזת. 

היכרות עם len היא חלק מחשיבה מכנית - חשוב לזכור שהיא מחזירה את מספר הבתים, לא התווים (במחרוזות Unicode).

## ראה גם:

- הדוקומנטציה של Go ל- [`len`](https://golang.org/pkg/builtin/#len)
- בלוג של Go בנושא [`Strings`](https://blog.golang.org/strings)
- הדוקומנטציה של Go ל- [`RuneCountInString`](https://golang.org/pkg/unicode/utf8/#RuneCountInString)