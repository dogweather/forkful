---
title:    "Go: השוואת שתי תאריכים"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה
השוואת שני תאריכים בתכנות ב-Go יכולה להיות כלי מקצועי ושימושי לכל מתכנת. באמצעות השוואה של שני תאריכים, ניתן לממש פונקציונליות רבה כגון ייחוס זמן, פילטרים תאריכים וכו'. במאמר הזה נלמד כיצד לבצע השוואת תאריכים בתוכנית שכתובה ב-Go.

## כיצד לעשות זאת
בכדי להשוות שני תאריכים בתוכנית ב-Go, ישנן כמה אפשרויות. האפשרות הראשונה היא על ידי שימוש בפונקציית `Equal` של חבילת `time`. הפונקציה מקבלת שני תאריכים ומחזירה אמת אם התאריכים זהים או שקר אם הם שונים. לדוגמא:

```Go
import (
    "fmt"
    "time"
)

func main() {
    date1 := time.Date(2020, 01, 01, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2020, 01, 01, 12, 0, 0, 0, time.UTC)

    if date1.Equal(date2) {
        fmt.Println("The two dates are equal")
    } else {
        fmt.Println("The two dates are not equal")
    }
}

// Output: The two dates are not equal
```

האפשרות השנייה היא להשתמש בפונקציית `Before` או `After` של חבילת `time`. פונקציית `Before` מחזירה אמת אם התאריך הראשון מופיע לפני התאריך השני ושקר אם התאריך הראשון מופיע אחרי התאריך השני. פונקציית `After` מחזירה אמת אם התאריך הראשון מופיע אחרי התאריך השני ושקר אם התאריך הראשון מופיע לפני התאריך השני. לדוגמא:

```Go
import (
    "fmt"
    "time"
)

func main() {
    date1 := time.Date(2020, 01, 01, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2021, 01, 01, 0, 0, 0, 0, time.UTC)

    if date1.Before(date2) {
        fmt.Println("Date1 is before date2")
    }

    if date2.After(date1) {
        fmt.Println("Date2 is after date1")
    }
}

// Output: Date1 is before date2
//         Date2 is after date1
```

## Deep Dive
בכדי לשווא שני תאריכים בצ