---
title:    "Go: קריאת קובץ טקסט"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

קריאת קובץ טקסט היא חלק חשוב בתהליך של פיתוח תוכניות בשפת גו. היא מאפשרת לנו לטעון מידע מקובץ טקסט ולעבד אותו בתוך התוכנית שלנו. במאמר זה, נלמד כיצד לקרוא קובץ טקסט בשפת גו ונתאר בצורה מעשית כיצד ניתן להשתמש בזה בהתאם לצרכים שלנו.

## איך לעשות זאת

כדי לקרוא קובץ טקסט בשפת גו, נצטרך להשתמש בפונקציה המתאימה, `ioutil.ReadFile()`. תחילה, נצטרך לייבא את החבילה `ioutil`, לאחר מכן נשתמש בפונקציה הזו כדי לקרוא את הקובץ ולשמור את התוכן שלו במשתנה. לדוגמה:

```Go
import(
  "fmt"
  "io/ioutil"
)

func main() {
  // קריאת קובץ טקסט ושמירת התוכן במשתנה "data"
  data, err := ioutil.ReadFile("file.txt")
  if err != nil {
    fmt.Println("אירעה שגיאה בקריאת הקובץ")
    return
  }
  // הדפסת התוכן של הקובץ
  fmt.Println(string(data))
}
```

כדי ליצור קובץ טקסט חדש וליצור בו תוכן, נשתמש בפונקציה `ioutil.WriteFile()` ונעבוד עם משתנה מסוג `[]byte`. לדוגמה:

```Go
import(
  "fmt"
  "io/ioutil"
)

func main() {
  // יצירת תוכן לקובץ חדש
  content := []byte("זהו התוכן שיהיה בקובץ החדש")
  
  // יצירת קובץ חדש ושמירת התוכן בו
  err := ioutil.WriteFile("new_file.txt", content, 0644)
  if err != nil {
    fmt.Println("אירעה שגיאה ביצירת הקובץ")
    return
  }
}
```

## חפר בעומק

פונקציות `ioutil.ReadFile()` ו- `ioutil.WriteFile()` מספקות פתרונות קלים עבור קריאת וכתיבת קבצים טקסט בשפת גו. טרם קריאת התוכן, נוכל לעבוד איתו במשת