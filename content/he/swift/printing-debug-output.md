---
title:                "הדפסת פלט מנתח"
html_title:           "Swift: הדפסת פלט מנתח"
simple_title:         "הדפסת פלט מנתח"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?

הדפסת הפלט של העתקה (debug output) היא תהליך שבו המתכנת מחזיק פונקציות או תנאים באמצעות הדפסות לקבלת מידע נוסף לגבי קוד שהוא מנסה להריץ. מתכננים משתמשים בתהליך זה בכדי לזהות בעיות ולפתור אותן בקלות.

## איך לעשות זאת:

דוגמאות לכתיבת קוד ופלט משתמשות בבלוקי קוד ```Swift ... ```.

```swift
// דוגמא לשימוש בפונקציה בכדי להדפיס מידע נוסף
func printDebugOutput() {
    print("מידע נוסף לגבי קוד זה")
}
// הפלט:
// מידע נוסף לגבי קוד זה
```

```swift
//דוגמא לכתיבת תנאים עם הדפסות למידע נוסף
let testScores = [85, 92, 77, 68, 95]
for score in testScores {
    if score >= 90 {
        print("ציון גבוה - \(score)")
    } else {
        print("ציון נמוך - \(score)")
    }
}
// הפלט:
// ציון גבוה - 92
// ציון גבוה - 95
// ציון נמוך - 77
// ציון נמוך - 68
// ציון גבוה - 85
```

## טיול עמוק:

ההדפסת הפלט של העתקה (Debug output) התחיל כאשר מתכנתים נאלצו להשתמש בו כדי למצוא באגים במכונות מכאניות. היום, יכולת זו הופכת את חיינו יותר קלים וסובלניים כי מאפשרת לנו לזהות בעיות ולתקן אותן בקלות בעזרת המידע הנוסף שאנחנו מקבלים. יתר על כן, ישנן כמה אלטרנטיבות להדפסת הפלט של העתקה, כגון בדיקת משתנים באמצעות בקרת בידור כמו דבאג ב Xcode. סביר להניח כי הדפסת הפלט של העתקה תישאר כלי מצוין עבור מתכנתים ומפתחים כאחד.

## ראו גם:

[מסמכי התיעוד של Swift](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)

[דניאל סטיימן על הדפסת הפלט של העתקה](https://danielsteiman.com/posts/when-to-use-prints-and-commented-code)