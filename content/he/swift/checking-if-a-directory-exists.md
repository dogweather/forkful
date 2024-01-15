---
title:                "לבדיקת קיום תיקייה בתוכנות מחשב"
html_title:           "Swift: לבדיקת קיום תיקייה בתוכנות מחשב"
simple_title:         "לבדיקת קיום תיקייה בתוכנות מחשב"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
מחקר ובדיקת קבצים ותיקיות הם חלק חשוב בכתיבת תוכניות לפיתוח. בדיקת האם תיקייה קיימת תוכל להגן מפני תקלות כגון ניסיון גישה לתיקייה שלא קיימת ולמנוע קריסות בתוכניות שלנו. על כן, חשוב לדעת כיצד ניתן לבדוק אם תיקייה קיימת בשפת סוויפט.

## איך לבדוק אם תיקייה קיימת
הליכים נוספים בקוד כדי לבדוק אם תיקייה קיימת:
``` Swift
let fileManager = FileManager.default
let directoryURL = URL(fileURLWithPath: "path/to/directory")

var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: directoryURL.path, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("\(directoryURL.path) היא תיקייה קיימת!")
    } else {
        print("\(directoryURL.path) היא לא תיקייה קיימת!")
    }
} else {
    print("תיקייה זו אינה קיימת!")
}
```

הקוד הראשוני יוצר אובייקט של מנהל הקבצים של המערכת. לאחר מכן, אנו יוצרים את נתיב התיקייה שאנו רוצים לבדוק. כאשר אנו משתמשים בפעולת `fileExists(atPath:isDirectory:)`, אנו מעבירים את הנתיב של התיקייה ומסמן לשיטה שנרצה לבדוק אם התיקייה היא באמת תיקייה אמיתית. הערך `isDirectory` משמש כמשתנה נוסף שאנו עוברים בהפעלה של הפעולה, כך שאנו יכולים לבדוק את הסוג של הפריט בהמשך.

## מעמקים
על מנת לעשות בדיקה מדויקת יותר, נוכל לדעת אם תיקייה קיימת על ידי שימוש בפעולת `contentsOfDirectory(at:includingPropertiesForKeys:options:)` של מנהל הקבצים. פעם שאנו מקבלים את רשימת הפריטים בדף העמוד הנותן של פעולת זאת, אנו יכולים להשוות את הפריטים המכילים בתיקייה לפריט שנכון לבדוק. 

## ראה