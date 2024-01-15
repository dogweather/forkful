---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Go: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה
3: אפשר להכנס למקרה בו אנחנו רוצים לאמת את הזהות של המשתמש שמביקש גישה למידע או שירות. לדוגמה, במקרים של כניסה לאתר או שימוש בAPI. 

## איך לבצע
השליחה של בקשת HTTP עם הזדהות בסיסית בשפת Go היא פשוטה ויעילה. המתחילים יוכלו להתחיל מהדוגמאות הבאות על מנת להבין את הקוד ולמי שיש ניסיון ישנן הכווניות מדויקות לפעולות המתאימות.

הנה דוגמה לשליחת בקשה POST באמצעות הזדהות בסיסית:

```Go
func main() {
    url := "https://example.com/api/users"
    body := []byte(`{"name": "John"}`)
    
    req, err := http.NewRequest("POST", url, bytes.NewBuffer(body))
               if err != nil {
                   log.Fatalln(err)
               }

    // הוספת הזדהות בסיסית לבקשה
    req.SetBasicAuth("username", "password")

    // שליחת הבקשה וקבלת התגובה
    client := http.Client{}
    resp, err := client.Do(req)
        if err != nil {
           log.Fatalln(err)
                }
    defer resp.Body.Close()

    // קריאת תוכן התגובה
    body, err := ioutil.ReadAll(resp.Body)
        if err != nil {
            log.Fatalln(err)
                }
    // הדפסת תוכן תגובת הבקשה
    fmt.Println(string(body))
}
```

תוצאה:

```
User John was successfully created.
```

כאן אנו שולחים בקשה POST לאתר "https://example.com/api/users" כדי ליצור משתמש חדש. אנו משתמשים בפונקציות המובנות של שפת Go כדי להגדיר את מבנה הבקשה ולשלוח אותה עם הזדהות בסיסית. לאחר מכן, אנו קוראים ומדפיסים את תוכן תגובת הבקשה.

## דיב המקבע
כאשר אנחנו משתמשים בהזדהות בסיסית, אנחנו חייבים לשלוח את שם המשתמש והסיסמה בכותרת "Authorization" של הבקשה. הערך של "Authorization" צריך להיות "Basic" ייתכן גם רווח ולאחר מכן מעקב לבינו בין : כתויות פשוטות בבסיס 64.

אם לדוגמה, בכת