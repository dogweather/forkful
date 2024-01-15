---
title:                "הורדת עמוד אינטרנט"
html_title:           "Go: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

תכנות ב-Go הוא מגוון רחב של בניית יישומים, בין היתר בתחומים שונים כגון רשתות ואינטרנט, עיבוד נתונים, ולמידה מתמטית. כאשר מגיע לשימוש ברשתות ואינטרנט, הפעולה הנפוצה ביותר היא טעינת דפי אינטרנט. כתבת זו תלמד אתכם כיצד להוריד דף אינטרנט בעזרת Go, כולל דוגמאות קוד והסברים מעמיקים כדי לעזור לכם להבין טוב יותר את התהליך.

## איך לעשות זאת

לטעינת דף אינטרנט ב-Go, נצטרך להשתמש בספריית "http" המציעה את הכלים הנחוצים עבור פעולות ברשת. נתחיל על ידי יצירת אובייקט של סוג "http.Client" על-ידי קריאה לפונקציה "http.NewClient()" והשמתו במשתנה שנקרא "client". לאחר מכן, נשתמש בפונקציה "client.Get()" על-ידי שליחת ה-URL של הדף הרצוי כפרמטר לפונקציה. כדי לקבל חזרה את תוכן הדף, נשתמש בפעולת "Response" על התוצאה של הפונקציה.

```Go
import "net/http"

func main() {
  resp, err := http.Get("https://www.example.com")
  if err != nil {
    fmt.Println(err)
    return
  }
  defer resp.Body.Close()

  body, err := ioutil.ReadAll(resp.Body)
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(string(body))
}
```

בדוגמא הזו, אנחנו רושמים את הפונקציה "http.Get()" כדי לטעון את תוכן הדף הרצוי. לאחר מכן, אנו משתמשים בפונקציה "ioutil.ReadAll()" כדי לקרוא בינארי את התוכן הטקסטואלי של הדף ולהדפיס אותו בעזרת הפונקציה "fmt.Println()". ככל שתפתחו יותר יישומים ב-Go, תלמדו כיצד להשתמש בספריית "http" ובפונקציות הנוספ