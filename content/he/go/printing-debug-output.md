---
title:    "Go: הדפסת פלט ניפוח"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה
נדרשת התכנסות כדי להדפיס פלט הופעת שגיאות.

## איך לעשות זאת
תוכלו להדפיס פלט הופעת שגיאות באמצעות פונקציות כמו "Println" או "Printf".

### לדוגמא:
```Go
func main() {
  num1 := 10
  num2 := 0
  result := num1 / num2

  fmt.Printf("Result: %d\n", result) // פלט: Result: 0, אנחנו מקבלים את הערך 0 כיוון שישנה חלוקה לא חוקית לפיותו מספר 0 
}
```

## Deep Dive
כדי להדפיס פלט הופעת שגיאות בגו, יש לעבוד עם החבילה "fmt". ניתן להשתמש בפונקציות כמו "Errorf" ו "Fprintf" לכתיבת הודעות תקליטריות. יחד עם זאת, ניתן להשתמש ב"Debug / log" בשביל התאמת רמת ההדפסה בין תהליכי הפיתוח והייצום.

## ראו גם
- [ספריית המכילה "פרטיים" של גו]("https://golang.org/pkg/log/")
- [מדריך לכתיבת בעיות וניתוח בגו]("https://techtraits.com/2020/05/01/go-debugging/")
- [כמה עצויות להדפסת הודעות בגו]("https://yourbasic.org/golang/how-to-capture-print-log-to-variable/")