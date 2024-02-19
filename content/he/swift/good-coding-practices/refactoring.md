---
aliases:
- /he/swift/refactoring/
date: 2024-01-26 03:37:51.833753-07:00
description: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05D4\u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05DE\u05D7\
  \u05E9\u05D1 \u05E7\u05D9\u05D9\u05DD \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\
  \u05D5\u05E0\u05D9\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\
  \u05EA \u05D0\u05EA \u05D1\u05E1\u05D9\u05E1 \u05D4\u05E7\u05D5\u05D3, \u05DC\u05E9\
  \u05E4\u05E8 \u05D0\u05EA \u05D4\u05E7\u05E8\u05D9\u05D0\u05D5\u05EA, \u05D9\u05DB\
  \u05D5\u05DC\u05EA\u2026"
lastmod: 2024-02-18 23:08:53.212361
model: gpt-4-0125-preview
summary: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05D4\u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05DE\u05D7\
  \u05E9\u05D1 \u05E7\u05D9\u05D9\u05DD \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\
  \u05D5\u05E0\u05D9\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\
  \u05EA \u05D0\u05EA \u05D1\u05E1\u05D9\u05E1 \u05D4\u05E7\u05D5\u05D3, \u05DC\u05E9\
  \u05E4\u05E8 \u05D0\u05EA \u05D4\u05E7\u05E8\u05D9\u05D0\u05D5\u05EA, \u05D9\u05DB\
  \u05D5\u05DC\u05EA\u2026"
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא התהליך של שינוי המבנה של קוד מחשב קיים ללא שינוי התנהגותו החיצונית. מתכנתים עושים זאת כדי לנקות את בסיס הקוד, לשפר את הקריאות, יכולת התחזוקה ולפרוש את הדרך לתכונות עתידיות עם חוב טכני מינימלי.

## איך לעשות:
בואו נתחיל עם דוגמה בסיסית ב-Swift שבה יש לנו קוד חוזר על עצמו:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("First Name: \(firstName)")
    print("Last Name: \(lastName)")
    print("Age: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Job Title: \(title)")
    print("Company: \(company)")
}
```

ריפקטורינג של זה כולל יצירת מבנה `User` כדי לאגד את תכונות המשתמש והוספת שיטה להדפסת הפרטים:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("First Name: \(firstName)")
        print("Last Name: \(lastName)")
        print("Age: \(age)")
        print("Job Title: \(jobTitle)")
        print("Company: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Software Developer", company: "Tech Solutions")
user.printDetails()
```

### פלט לדוגמה:
```
First Name: John
Last Name: Doe
Age: 30
Job Title: Software Developer
Company: Tech Solutions
```

## צלילה עמוקה
ריפקטורינג יש שורשים שחוזרים לימי ההתחלה של הנדסת תוכנה, אבל המונח התפרסם באופן מיוחד בסוף שנות ה-90, בעיקר דרך הספר המכונן של מרטין פאולר "Refactoring: Improving the Design of Existing Code". הספר הניח את העיקרון שקוד צריך להיות מנוקה באופן רציף בצעדים קטנים במקום לחכות לשלב נפרד.

חלופות לריפקטורינג ידני כוללות כלים אוטומטיים וסביבות פיתוח משולבות (IDEs) שיכולות לעזור לגלות קוד כפול, להציע פשטות וליצור אוטומטית חלקים מהקוד. Xcode, לפיתוח Swift, מציע מגוון כלים לריפקטורינג, כמו שינוי שם ויצירת שיטה שיכולים להפחית את הפוטנציאל לטעויות אנוש.

כשמיישמים ריפקטורינג, חשוב לדעת שיש סוויטת בדיקות מוצקה במקום. הבדיקות משמשות כרשת ביטחון, ומבטיחות שהשינויים שאתם מבצעים לא מביאים להופעת באגים. זה חיוני מכיוון שהמטרה העיקרית של ריפקטורינג היא לשנות את המבנה הפנימי ללא השפעה על ההתנהגות החיצונית.

## ראה גם
- ["Refactoring: Improving the Design of Existing Code" של מרטין פאולר](http://martinfowler.com/books/refactoring.html)
- [תיעוד Swift מאת אפל](https://swift.org/documentation/)
- [שימוש בכלי ריפקטורינג של Xcode](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [מדריך סגנון Swift של Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
