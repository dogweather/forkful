---
title:                "כתיבת מבחנים"
html_title:           "Swift: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-tests.md"
---

{{< edit_this_page >}}

למה נכתוב בדיקות?

בדיקות הם חלק חשוב מתהליך הפיתוח התוכנה שלנו. הן מאפשרות לנו לבדוק את תיקוני הבעיות ואת פעולת התוכנה שלנו לפני שהיא נכנסת לשוק. כמו כן, הן מסייעות לנו למצוא באופן מוקדם את השגיאות ולמנוע מהן להווכח תוך כדי שימוש בתוכנה.

## איך לבצע:

לפניכם כמה דוגמאות קוד ותצוגת פלט עם שימוש בבדיקות בשפת Swift:

```Swift
// דוגמה 1:
func calculateArea(length: Int, width: Int) -> Int {
    return length * width
}

let length = 10
let width = 5
let expectedArea = 50

let area = calculateArea(length: length, width: width)

assert(area == expectedArea, "השטח צריך להיות 50")

// תצוגת פלט: אין דייקנות

// דוגמה 2:
func checkPalindrome(string: String) -> Bool {
    return string.lowercased() == String(string.lowercased().reversed())
}

let inputString = "קאבאק"
let expectedOutput = true

let output = checkPalindrome(string: inputString)

assert(output == expectedOutput, "המחרוזת אמורה להיות פלינדרום")

// תצוגת פלט: אין דייקנות
```

## כידוע:

בעבר, בדיקות נכתבו בידניות על ידי המתכנתים. אך עם התפתחות הטכנולוגיות, נוצרו כלים פתוחים ומסגרות נגזרות שמאפשרים כתיבת בדיקות אוטומטיות. כמו כן, ישנן אפליקציות שמתאפשרות בדיקת קוד על ידי עורך הקוד עצמו.

## לצפייה נוספת:

למידע נוסף על בדיקות בשפת Swift, ראו רשימת המקורות המצורפת.

## ערוך את הקוד שלך עם ביטחון:

כעת, שאתם מכירים את הטכניקות לכתיבת בדיקות בשפת Swift, כדאי להשתמש בהן כדי להבטיח שהתוכנה שלכם עובדת כראוי. בדיקות מעוניינות את התוכנה שלכם מבחינת עבודתה ותוכניות תומכות במגוון רחב של אופני מבחן, מה שמאפשר לכם להיות בטוחים שהקוד שלכם עובד כמצופה. אז אל תתעצלו ותכינו בדיקות עבור הקוד שלכם. זה ייכנס לשימוש מספר פעמים ויחסוך לכם זמן וכאבי ראש בעתיד!