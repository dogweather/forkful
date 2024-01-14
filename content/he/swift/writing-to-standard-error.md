---
title:                "Swift: כתיבה לשגיאת תקן"
simple_title:         "כתיבה לשגיאת תקן"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

מדוע כדאי לכם לכתוב לפלט שגיאות סטנדרטי?

## איך לכתוב

כדי לכתוב לפלט שגיאות סטנדרטי בשפת סוויפט, תוכלו להשתמש בפונקציה "fputs". הנה דוגמה לכתיבת שגיאת "הכנסיות" לפלט סטנדרטי על ידי שימוש בפונקציה המתאימה לכך:

``` Swift
fputs("חרף ציפורי הסתיו", stderr)
```

כאשר נריץ את הקוד הנ"ל, נקבל את הפלט הבא:

`חרף ציפורי הסתיו`

## חפירה עמוקה

כאשר אתם כותבים לפלט שגיאות סטנדרטי בשפת סוויפט, יתרומנה כי אתם יכולים להשתמש בפונקציה "fprintf" במקום במקום בפונקציה "fputs". פונקציה זו מאפשרת לכם להוסיף מידע נוסף לפלט השגיאות. לדוגמה:

``` Swift
let name = "שלום"
let age = 30

fprintf(stderr, "היי, שמי הוא %s ואני בן %d", name, age)
```

הפלט הוא:

`היי, שמי הוא שלום ואני בן 30`

## ראו גם

* [מדריך לשגיאות סטנדרטי בשפת סוויפט](https://www.swiftbysundell.com/articles/standard-errors-in-swift/)
* [פונקציות פלט סטנדרטיות בשפת סוויפט](https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID102)
* [מדריך מלא לתחום התכנות בשפת סוויפט](https://developer.apple.com/swift/resources/)