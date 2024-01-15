---
title:                "התחלת פרויקט חדש"
html_title:           "Swift: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

אחת הדברים הכי משמעותיים בכתיבת קוד ב-Swift הוא היכולת ליצור פרויקטים חדשים בקלות. זה מאפשר לפתחים ליצור יישומים חדשים ומתוחכמים ביעילות רבה יותר מאשר משקל עם פרויקטים קיימים.

## How To

הרכיב הכי חשוב בכתיבת קוד הוא להתחיל בפרויקט חדש. נדות דוגמאות של מבנה הפרויקט המומלץ:

```
struct NewProject {
    
    var title: String
    var description: String
    var dateCreated: Date
    var creator: String
    
    func printInfo() {
        print("Title: \(title)")
        print("Description: \(description)")
        print("Date Created: \(dateCreated)")
        print("Creator: \(creator)")
    }
    
}
```
לאחר הגדרת מבנה הפרויקט, ניתן ליצור מופע שלו ולקבל גישה לכל אחד מהפרטים:

```
let newProject = NewProject(title: "פרויקט חדש",
                            description: "פרויקט זה מתאר יישום חדש שאנחנו בונים ב-Swift",
                            dateCreated: Date(),
                            creator: "שירות המעצבים")

newProject.printInfo()

// Output:
// Title: פרויקט חדש
// Description: פרויקט זה מתאר יישום חדש שאנחנו בונים ב-Swift
// Date Created: 2021-09-14 18:30:00 +0000
// Creator: שירות המעצבים
```

כעת, נוכל להתחיל להוסיף פונקציונליות נוספת לפרויקט כגון תנאים, לולאות ועוד. כדי לעזור לנו לשמור על קוד נקי וברור, חשוב להשתמש בשמות משמעותיים למשתנים ופונקציות ולהעביר הערות כאשר נחוצה.

## Deep Dive

להלן כמה טיפים נוספים ליצירת פרויקט חדש ב-Swift:

- השתמשו ב-Xcode כדי ליצור את המבנה הבסיסי של הפרויקט ולקלוע את המבנה שלו.
- חלקו את הקוד לפונקציות מובנות כדי לקלוע את הקוד שלכם ולהפחיד את הקוד.
- חשוב להקצות זמן ל