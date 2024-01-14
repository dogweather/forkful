---
title:                "Go: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-json.md"
---

{{< edit_this_page >}}

## למה

JSON הוא פורמט קוד שנמצא בשימוש נרחב בתכנות בשפת גו. אולם, לעיתים קשה להבין למה נדרש שימוש בו ומה היתרונות שלו. בפוסט הזה נבין מדוע חשוב להכיר את עולם ה-JSON וכיצד ניתן לעבוד איתו בפורמט הקוד של גו.

## איך לעבוד עם JSON בגו

כדי לעבוד עם JSON בגו, עלינו להתחיל עם החבילות הנדרשות. נוסיף למעבדה שלנו את הטכנולוגיות הבאות:

```Go
import (
    "encoding/json"
    "fmt"
)
```

כעת, נגדיר משתנה המכיל נתוני משתמש שמיוצגים בפורמט JSON:
```Go
var user = `{"name": "John", "id": 1234, "email": "john@example.com"}`
```

עכשיו נבקש מגו להמיר את הנתונים האלו למבנה מובן יותר עם עזרת פונקציית `json.Marshal()`:
```Go
data, err := json.Marshal(user)
if err != nil {
    fmt.Println(err)
}
```

כעת, נדפיס את הפלט בעזרת `fmt.Println`:
```Go
fmt.Println(string(data))
```

הפלט הוא מבנה נוקשה יחסית, אך אם נריץ את הקוד נקבל את התוצאה הבאה:
`"{\"name\":\"John\",\"id\":1234,\"email\":\"john@example.com\"}"`

אם נרצה לקרוא מידע בפורמט JSON ניתן לעשות זאת באמצעות פונקציית `json.Unmarhsal()`:
```Go
userData := make(map[string]string)
err = json.Unmarshal(data, &userData)
fmt.Println("Name:", userData["name"])
fmt.Println("ID:", userData["id"])
fmt.Println("Email:", userData["email"])
```

פלט:
```
Name: John
ID: 1234
Email: john@example.com
```

באמת שפשוט לעבוד עם JSON בגו!

## שוקולד עוסק בJSON בעומק

מאחר ו-JSON הוא פורמט קוד פשוט יחסית למחשבים, רבים מתעלמים מהעובדה שישנם מאפיינים מתוחכמים ומועילים בפורמט זה. למשל, תמיכה בסוגי נתונים שונים, יצירת מבני נתונים מקוננים וכו'.

כדי להעמיק בנושא וללמ