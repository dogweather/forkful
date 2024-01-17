---
title:                "עבודה עם json"
html_title:           "Go: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-json.md"
---

{{< edit_this_page >}}

##

## מה ולמה?
עבודה עם JSON היא תהליך שבו מתאגדים נתונים בפורמט קורא עבור סיירים אינטרנט. תהליך זה נעשה כדי להעביר מידע בצורה יעילה וקריאה הרבה יותר עבור מכשירים אלקטרוניים. תהליך זה יעיל במיוחד עבור פיתוחנים, שמנסים לבנות אפליקציות או אתרי אינטרנט.

## איך לעשות זאת:
קוד דוגמה עם הפלט עבור שניים מסוגי התצוגה
```Go 
type Location struct { 
  Name string `json:name` 
  Country string `json:country, omitempty` 
}

type User struct {
  Username string
  Name string
  Birthday Time `json:birthday`
}

joe := User{Username: "joe123", Name: "Joe Smith", Birthday: time.Now()}
encodedJoe, _ := json.Marshal(joe)
fmt.Println(encodedJoe)
// Output: {"username":"joe123", "name":"Joe Smith", "birthday":"2021-07-13T00:00:00Z"}
```

## כייטור לעומק:
תהליך זה נוצר כדי להיות אלטרנטיבה לשימוש בווב סרוויס רב פיי. הפורמט הנוכחי שלו נוצר בשנת 2002 על ידי דאג קאסטלטון וכריס סאלסו. חזרה אז זה הייתה פורמט מאוד פשוט לנהל בשילוב עם ריאק.

## ראו גם:
- [המאמר המקורי על JSON](https://www.json.org/)
- [עמוד הוויקי על JSON באתר שפות] רשת עם רְאוּי וטיי)
- [המדריך הרשמי של גו על פורמט JSON]רג'קט סאפלי נוצר על ידי),