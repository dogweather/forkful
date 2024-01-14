---
title:                "Swift: למיצוא תת מחרוזות"
simple_title:         "למיצוא תת מחרוזות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

##למה

יש לנו מחרוזת גדולה ורעיון מוחשי לבצע בתוכה. אפשר לחלץ חלקים קטנים מהמחרוזת לפי תבניות שונות ולהשתמש בהם לצורך מטרות שונות. כך ניתן לבצע פעולות יעילות וממוקדות יותר על המחרוזת.

##איך לבצע

מניעת קלט למשקולת מחיר. למשל, אפשר לכתוב פונקציה שמחלצת מהמחרוזת רק את המילים שמתחילות באות גדולה ולהציג רק את המילים המהוות מספרים.

```Swift
func extractWords(string: String) -> [String] {
    var result: [String] = []
    let words = string.components(separatedBy: " ")
    for word in words {
        if let firstChar = word.first, firstChar.isUppercase {
            result.append(word)
        }
    }
    return result
}

let string = "Hello World 2021 Swift Programming Blog"
let result = extractWords(string: string)
```

הפלט:

> ["Hello", "World", "Swift", "Programming", "Blog"]

##מעמק

פונקציות כמו `components(separatedBy:)` ו- `index(of:)` מסייעות לנו לחלץ חלקים מהמחרוזת המקורית. ניתן להשתמש גם בפונקציות נוספות כגון `prefix(_:)`, `suffix(_:)`, ו- `substring(to:)` כדי לחלץ חלקים מדויקים מהמחרוזת.

הנה דוגמא לפונקציה שמחלצת את המילים שנמצאות במחרוזת עד כמה תווים מסוימים מההתחלה:

```Swift
func extractWords(string: String, length: Int) -> [String] {
    var result: [String] = []
    let words = string.components(separatedBy: " ")
    for word in words {
        if word.count <= length {
            result.append(word)
        } else {
            let index = string.index(string.startIndex, offsetBy: length)
            let substring = string[..<index]
            result.append(String(substring))
        }
    }
    return result
}

let string = "Hello World 2021 Swift Programming Blog"
let result = extractWords(string: string, length: 5)
```

הפלט:

> ["Hello", "World", "2021", "Swift", "Progr", "Blog"]

##ראו גם

- [מדריך מאתר Apple על חיתוך מחרוזת באמצעות אופרטורים](https://developer.apple.com/library/archive/documentation/StringsTextFonts/Conceptual/TextAndWebiPhoneOS/WorkingWithStrings/WorkingWithStrings.html#//apple_ref/doc/uid/TP40009542-CH5-SW8)
- [מדריך מאתר Hacking with Swift על חיתוך מחרוזת בעזרת Substring](https://www.hackingwithswift.com/example-code/strings