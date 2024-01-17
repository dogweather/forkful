---
title:                "קריאת קובץ טקסט"
html_title:           "Swift: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה? 
קוראים לקובץ טקסט זה לשיטה שמאפשרת למתכנתים לקרוא ולהתייחס לתוכן שנמצא בקובץ מסוים. קריאת קבצי טקסט היא חלק בלתי נפרד מתחום התכנות כי היא מאפשרת כניסה ועיבוד של מידע שממוקד סביב מחשבים.

## איך לעשות: 
באמצעות שפת Swift, ניתן לקרוא תוכן שנמצא בקובץ טקסט כמו שאתה עושה עם קבצים אחרים. השימוש בפונקציות פנימיות כמו "readLine ()" תאפשר לך לקרוא את התוכן המעניין ולהתייחס אליו לפי הצורך. לדוגמה, אם תרצה לקרוא קובץ טקסט שמכיל את שם האקונט שלך, תוכל לעשות זאת על ידי הרצת הפקודה הבאה:

```Swift
if let filePath = Bundle.main.path(forResource: "account", ofType: "txt"),
let fileContent = try? String(contentsOfFile: filePath) {
    print("Your account name is \(fileContent)")
}
```

התוכן שנקרא יופיע בתוך משתנה בשם "fileContent" שאליו אתה יכול להתייחס בהמשך התכנית שלך.

## חפירה עמוקה: 
קריאת קובץ טקסט אינה תהליך חדש וגם לא סובכת כמו שהיא בימי קדם שהיה למתכנתים ליצור תוכניות נפרדות להתמודד מול כל סוג של קובץ טקסט. היום ישנם כלים מתקדמים יותר שמסייעים למתכנתים לקרוא ולעבד תוכן של קבצי טקסט, כגון ספריות חיצוניות ופונקציות פנימיות בשפת Swift כמו String(contentsOfFile:), which allows the programmer to deal with the content directly instead of dealing with low-level operations such as file descriptors and buffers. 
למידע נוסף על פונקציות קריאת קבצי טקסט ניתן לקרוא את התיעוד אודות הפונקציה "String(contentsOfFile:)" ולראות דוגמאות שונות של קוד מקור לטיפול בקבצי טקסט.

## ראה גם: 
למידע נוסף על קיפודית קריאת קבצי טקסט בשפת Swift ראה את התיעוד הרשמי של לשונית "קריאה" בדפדפן קיפודית השפה הרשמי.