---
title:                "טיפול בשגיאות"
date:                  2024-01-26T00:59:46.242720-07:00
model:                 gpt-4-1106-preview
simple_title:         "טיפול בשגיאות"

category:             "Swift"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/handling-errors.md"
---

{{< edit_this_page >}}

## מה ולמה?
טיפול בשגיאות ב-Swift פירושו לצפות ולהגיב לבעיות שמופיעות כאשר הקוד שלך רץ. אנחנו עושים את זה כדי לשלוט בתוהו ובוהו - למנוע מאפליקציות לקרוס ולתת למשתמש חוויה חלקה.

## איך לעשות:
Swift משתמשת בטיפול שגיאות עם בלוקים של `do`, `try`, ו-`catch`. בואו נסתכל:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // נדמה שיש לנו כאן קצת לוגיקה לבדוק אם קובץ קיים ואם יש לנו הרשאה לקרוא אותו
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "תוכן הקובץ יהיה כאן"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("אופס! הקובץ לא נמצא.")
} catch FileError.noPermission {
    print("אה! אין הרשאה לקרוא את הקובץ.")
} catch {
    print("אירעה שגיאה לא ידועה.")
}

```

פלט לדוגמה:

```
אופס! הקובץ לא נמצא.
```

## צלילה עמוקה
טיפול בשגיאות לא תמיד היה מושחל כמו שהוא עכשיו. ב-Objective-C, היית עוסק עם מצביינים לאובייקטים NSError, שהרגישו מגושמים. עכשיו, יש לנו מערכת יותר אלגנטית עם enums של Swift והפרוטוקול `Error`.

הפקודה `throw` של Swift מאפשרת לנו לאותת שמשהו השתבש. בלוקי `do` פועלים כמו תחומים מודעים לשגיאות, `try` מסמן קריאות בעייתיות, ו-`catch` מטפל בכל דבר אם הוא הולך דרום.

אופציונלים הם חלופה עבור מצבים שאינם דווקא בסטטוס "שגיאה" אבל עדיין עלולים לא להכיל "תוצאה". הם מעין משתנים של שרדינגר - יש להם ערך או שאין להם.

לעומק אמיתי, בדקו את טיפוסי `Result`, שהם היברידים שיקיים בין תבניות תוצאה רגילה לתבניות שגיאה.

## ראה גם
- מדריך הטיפול בשגיאות הרשמי של Swift: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- מיטב התרגילים לטיפול בשגיאות ב-Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- טיפול מתקדם בשגיאות ב-Swift: [מאמר ב-Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
