---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Swift: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

למה לנו להמיר מחרוזת לאותיות קטנות? אחד הסיבות העיקריות היא כדי להשוות בין טקסטים בצורה יעילה יותר. כאשר המחרוזת שלנו מכילה אותיות גדולות ואותיות קטנות, היא יכולה להוות תהליך מייגע של השוואת טקסטים, כיוון שהמחשב לא מכיר בין אותיות גדולות וקטנות כאלו.

## איך לכתוב קוד

נתחיל עם דוגמא פשוטה בשפת סוויפט כדי להמיר מחרוזת לאותיות קטנות. 

```Swift
let text = "HELLO, WORLD!"
let lowerCaseText = text.lowercased()
print(lowerCaseText)
```

תוצאה: 
hello, world!

ניתן לראות כי הפונקציה lowercased () משנה את המחרוזת לאותיות קטנות ומחזירה את התוצאה המעודכנת. כאן, התוצאה המעודכנת נשמרת במשתנה lowerCaseText ונדפסת כדי שנוכל לראות את השינוי בטקסט.

עכשיו, נראה דוגמא קצת יותר מורכבת ופונקציונלית יותר.

```Swift
func convertToLowerCase(text: String) -> String {
    let result = text.lowercased()
    return result
}

let text = "This Is A Long Text"
let lowerCaseText = convertToLowerCase(text: text)
print(lowerCaseText)
```

תוצאה:
this is a long text

הפונקציה convertToLowerCase () מקבלת מחרוזת כפרמטר ומשנה את האותיות לקטנות במשתנה תוצאה. התוצאה המעודכנת מוחזרת ונשמרת במשתנה lowerCaseText.

## נכנס לעומק

המרת מחרוזת לאותיות קטנות בשפת סוויפט היא פעולה פשוטה ואחת הפונקציות הנפוצות בשימוש ביום-יום. כדי להבין את התהליך והטכניקות שסביבו, חשוב להבין כמה נקודות חשובות:

- פונקציה זו מחזירה את התוצאה המעודכנת, אך לא משנה באופן ישיר את המחרוז