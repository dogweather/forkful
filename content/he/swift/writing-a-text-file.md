---
title:                "כתיבת קובץ טקסט"
html_title:           "Swift: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כשמתחילים לפתח בשפת תכנות Swift, המטרה העיקרית היא ליצור יישום עבור מכשירי iOS או macOS. לכן, כתיבת קבצי טקסט יכולה להיות כלי עיקרי כדי לאחסן מידע וליצור תקשורת בין המשתמש ליישום.

## כיצד לעשות זאת

כדי לכתוב קובץ טקסט ב-Swift, ניתן להשתמש בפונקציה המובנית "write(to:atomically:encoding)" כדי לכתוב מחרוזת אל תוך קובץ ולשמור אותו. לדוגמה, אם נרצה ליצור קובץ חדש ולכתוב לו, נוכל להשתמש בקוד הבא:

```Swift
let string = "זהו טקסט שרצינו לכתוב לקובץ טקסט"
let url = URL(fileURLWithPath: "myfile.txt")

do {
    try string.write(to: url, atomically: true, encoding: .utf8)
    print("קובץ טקסט נכתב בהצלחה!")
} catch {
    print("אירעה שגיאה בכתיבת הקובץ")
}
```

כדי לקרוא קובץ טקסט שכבר קיים, ניתן להשתמש בפונקציה המובנית "String(contentsOf:usedEncoding)" כדי לקרוא את תוכן הקובץ כמחרוזת. לדוגמה, אם נרצה להדפיס את התוכן של קובץ טקסט שנכתב בקוד שהראינו לעיל, נוכל להשתמש בקוד הבא:

```Swift
let url = URL(fileURLWithPath: "myfile.txt")

do {
    let text = try String(contentsOf: url, usedEncoding: .utf8)
    print(text)
} catch {
    print("אירעה שגיאה בקריאת הקובץ")
}
```

הפונקציות הנ"ל הן רק חלק מהאפשרויות לכתיבה וקריאה של קבצי טקסט ב-Swift. ניתן להשתמש גם בכליים נוספים כגון "FileManager" ו-"Data" כדי לנהל קבצים ותיקיות ולהתמודד עם תהליכי מחיקה והעתקה של קבצים.

## נחקור עוד

בנוסף למבואר לעיל, אפשר למ