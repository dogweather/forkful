---
title:                "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כתיבת קוד היא כדלקמן משאב מפתח לכל תכנית מחשב. תיבת טקסט היא אחת היכולות הבסיסיות שכדאי לדעת כדי לייצר תכניות מתקדמות בגו.

## איך לעשות

מטרת המאמר הזה היא ללמוד לקרוא קובץ טקסט בשפת גו. נתחיל עם קצת קוד בסיסי, ואחר כך נדרוש יותר עמק בכדי להבין את התהליך הרקורסיבי שמאפשר לנו לקרוא קבצים גדולים.

```Go
func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```

הקוד מתחיל עם פתיחת קובץ קיים בעזרת הפונקציה os.Open. אם הפונקציה מחזירה שגיאה, נעביר את השגיאה לפונקציה log.Fatal כדי לסיים את התוכנית. ברגע שהקובץ נפתח, ניצור אובייקט של bufio.Scanner שיעבוד על הקובץ הנפתח. למעשה, הסורק יקרא כל שורה בקובץ עם הפונקציה Scan. ובכל שורה שהקורא בוחן, הוא יודפס למסך בעזרת הפונקציה fmt.Println.

כדי לוודא שאין שגיאות, נציג את בדיקת השגיאות err המתאימה לסורק. אם נתקלנו בשגיאה, נעביר אותה לפונקציה log.Fatal כדי לסיים את התוכנית.

כעת, היכנסו למתכונת הרקורסיבית בשפת גו כדי לאפשר קריאה נמוכה לקבצים גדולים.

```Go
func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    fileDetails, err := file.Stat()
    if err != nil {
        log.Fatal(err)
    }

    fileSize := fileDetails.Size()
    buffer := make([]byte, fileSize)
    bytes := bufio.NewReader(file)
    _, err = bytes.Read(buffer)

    fmt.Println(buffer)
}
```

בקוד הזה, אנו משתמשים בפונק