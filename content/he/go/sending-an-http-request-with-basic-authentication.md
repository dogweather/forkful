---
title:                "Go: שליחת בקשת HTTP עם הפעלה בסיסית"
simple_title:         "שליחת בקשת HTTP עם הפעלה בסיסית"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה
ישנם כמה סיבות שיכולות להוביל לתקשורת בין שני מערכות באמצעות בקשת HTTP עם אימות בסיסי. למשל, כאשר מערכת אחת צריכה לקבל גישה למידע ממערכת אחרת שמגישה אותו באופן מוגן באמצעות אימות בסיסי. בקשת HTTP עם אימות בסיסי מאפשרת למערכת לזהות את המשתמש והסיסמה שלו כדי לאמת את הגישה למידע.

## איך לבצע
כדי לשלוח בקשת HTTP עם אימות בסיסי באמצעות שפת תכנות Go, נצטרך להשתמש בחבילת "net/http" ולהשתמש בפונקציית "NewRequest" כדי ליצור בקשה. בקשה זו תכיל את הכותרת "Authorization" עם ערכי משתמש וסיסמה מתאימים. להלן דוגמא לשליחת בקשה עם אימות בסיסי:

```Go
req, err := http.NewRequest("GET", "https://example.com/api/endpoint", nil)
if err != nil {
    fmt.Println("Error creating request: ", err)
    return
}

// Set basic authentication
req.SetBasicAuth("username", "password")

resp, err := http.DefaultClient.Do(req)
if err != nil {
    fmt.Println("Error sending request: ", err)
    return
}

defer resp.Body.Close()

// Print response status code and body
fmt.Println("Response status: ", resp.Status)
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    fmt.Println("Error reading response body: ", err)
    return
}
fmt.Println("Response body: ", string(body))
```

פלט הדוגמא יהיה:

```
Response status: 200 OK
Response body: {"message": "success"}
```

## לחקור יותר
כדי להבין יותר מה קורה מאחורי הקלעים כאשר אנו שולחים בקשת HTTP עם אימות בסיסי, ניתן לחקור יותר על הנושא. ישנם עוד טיפולים שניתן לבצע על בקשה כגון הוספת כותרת "WWW-Authenticate" לתגובת השגיאה. בנוסף, כדי למנוע חשיפת מידע רגיש בקריאת העמוד HTTP, ניתן להשתמש בחיבור מאובטח (HTTPS).

## ראו גם
- [חבילת "net/http" בגו](https://golang.org/pkg/net/http/)
- [Wonderingחשובמשהו ספציפי? צור בקשת HTTP עם