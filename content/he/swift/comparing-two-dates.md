---
title:                "השוואת שני מועדים"
html_title:           "Swift: השוואת שני מועדים"
simple_title:         "השוואת שני מועדים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה
אנשים עשויים להשתמש בהשוואת שתי תאריכים בכדי לבדוק אם התאריכים תואמים או מי נמוך יותר מהאחר.

## כיצד לעשות זאת
כדי לבצע השוואה בין שני תאריכים בשפת Swift ישנם כמה דרכים שיכולות לעזור לנו. נבחר בצורה קלה ונוחה עם עזרת הפונקציה `compare()`.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"

let date1 = dateFormatter.date(from: "01/01/2020")
let date2 = dateFormatter.date(from: "01/02/2020")

if let unwrappedDate1 = date1, let unwrappedDate2 = date2 {
    if unwrappedDate1.compare(unwrappedDate2) == .orderedAscending {
        print("\(unwrappedDate1) קטן מ־\(unwrappedDate2)")
    } else {
        print("\(unwrappedDate1) גדול מ־\(unwrappedDate2)")
    }
}
```
כמו שאתם יכולים לראות, אנו משווים בין שני תאריכים באמצעות הוראת מטמון IF לבדוק האם התאריכים תואמים או מי גדול יותר מהשני.

## נכנסים לעומק
כדי להבין טוב יותר את תהליך ההשוואה בין שתי תאריכים, נשתמש במבנה DateComponents שיעזור לנו לפנות ליחידות זמן מסוימות בתאריך.

```Swift
let calendar = Calendar.current
let components1 = calendar.dateComponents([.day, .month, .year], from: unwrappedDate1)
let components2 = calendar.dateComponents([.day, .month, .year], from: unwrappedDate2)
```

לאחר מכן, אנו יכולים להשתמש בפונקציה `compare(_:)` על מנת להשוות בין המרכיבים של שני התאריכים.

```Swift
let dateComparison = calendar.compare(components1, to: components2, toGranularity: .day)
```

בסופו של דבר, החזרנו שלושה תוצאות אפשריות לשימוש בהשוואה בין שתי תאריכים:

- `.orderedAscending`: אם התאריך הראשון קטן מהתאריך השני.
- `.orderedSame`: אם התאריךים תואמים.
- `.orderedDescending`: אם התאריך הראשון גדול מהתאריך השני.

## ראו גם
- [מדריך לשפת Swift](https://developer.apple.com/swift/resources/)
- [מסמכי מדריכים רשמיים של Swift](https://developer.apple.com/documentation/swift)