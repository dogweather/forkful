---
date: 2024-01-26 01:10:05.342305-07:00
description: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05DC\u05D5\u05D2\u05D9\u05DD \u05D4\
  \u05D5\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05EA\u05D9\u05E2\
  \u05D5\u05D3 \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05D9\u05D5\u05EA \u05D9\u05D9\
  \u05E9\u05D5\u05DE\u05D9\u05DD, \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D5\u05DE\
  \u05D9\u05D3\u05E2 \u05D7\u05E9\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05DC\u05D0\u05DE\
  \u05E6\u05E2\u05D9 \u05E7\u05D9\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05E7\u05D5\u05D1\
  \u05E5 \u05D0\u05D5 \u05DE\u05E1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\
  \u05D9 \u05D4\u05D1\u05E8\u05D9\u05D0\u05D5\u05EA\u2026"
lastmod: '2024-02-25T18:49:38.158009-07:00'
model: gpt-4-1106-preview
summary: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05DC\u05D5\u05D2\u05D9\u05DD \u05D4\u05D5\
  \u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05EA\u05D9\u05E2\u05D5\
  \u05D3 \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05D9\u05D5\u05EA \u05D9\u05D9\u05E9\
  \u05D5\u05DE\u05D9\u05DD, \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D5\u05DE\u05D9\
  \u05D3\u05E2 \u05D7\u05E9\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05DC\u05D0\u05DE\u05E6\
  \u05E2\u05D9 \u05E7\u05D9\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05E7\u05D5\u05D1\u05E5\
  \ \u05D0\u05D5 \u05DE\u05E1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD. \u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\
  \u05DC \u05DE\u05E0\u05EA \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9\
  \ \u05D4\u05D1\u05E8\u05D9\u05D0\u05D5\u05EA\u2026"
title: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA (\u05DC\
  \u05D5\u05D2\u05D9\u05DD)"
---

{{< edit_this_page >}}

## מה ולמה?
רישום לוגים הוא התהליך של תיעוד התנהגויות יישומים, שגיאות ומידע חשוב אחר לאמצעי קיים, כמו קובץ או מסד נתונים. תכנתים עושים זאת על מנת לעקוב אחרי הבריאות והביצועים של האפליקציות שלהם, לנפות בעיות, ולשמור עין על מה שקורה "מתחת למכסה" בסביבות ייצור.

## איך לעשות:
בשפת Swift, אפשר לכתוב לוגים לקונסול עם פקודות print או עם ה-API הגמיש יותר, `os.log`, אשר מתחבר למערכת הרישום המאוחדת בפלטפורמות של Apple.

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // פקודת print פשוטה
    print("התחיל הורדת נתונים")
    
    // רישום אירוע ברמת מידע באמצעות os.log
    os_log(.info, log: logger, "מוריד נתונים מ-API.")
    
    do {
        let data = try performNetworkRequest()
        // רישום אירוע ברמת ניפוי
        os_log(.debug, log: logger, "נתונים נתפסו: %@", data.description)
    } catch {
        // רישום אירוע ברמת שגיאה
        os_log(.error, log: logger, "נכשל בהורדת נתונים: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // סימולציה של בקשת רשת
    return Data()
}
```

פלט לדוגמה בקונסול עשוי להיראות כך:

```
התחיל הורדת נתונים
מוריד נתונים מ-API.
נתונים נתפסו: כמה בתים של נתונים...
```

עבור שגיאות, זה יכול להיראות כך:

```
נכשל בהורדת נתונים: נראה שהחיבור לאינטרנט אינו פעיל.
```

## טבילה עמוקה
רישום לוגים ב-Swift מקבל עוצמה ויעילות חדשות עם מערכת הרישום המאוחדת שהוצגה ב-iOS 10 ו-macOS Sierra. בניגוד לפקודת `print` שהולכת ישירות לקונסול, מערכת זו מבוססת פעילות, ומאפשרת לך לסנן הודעות לוג על פי חשיבותן והאם הן מהוות בניות ניפוי או שחרור.

ההקשר ההיסטורי משקף את התפתחות רישום הלוגים ב-iOS ו-macOS מפקודות פרינט פשוטות לכלים מקיפים המשתלבים עם אפליקציית Instruments ו-Console, ומספקים דרכים מתוחכמות לניתוח לוגים.

ישנם מגוון חלופות לרישום לוגים ב-Swift, כמו הספריות של צד שלישי כמו CocoaLumberjack, אשר מציעה שכבת פקודות מעל מערכת הרישום המאוחדת. היא מספקת שליטה משופרת על עיצוב הלוג, ניהול קבצים ואפשרויות ביצועים.

לבסוף, פרטי היישום; OSLog תוכננה לא רק להיות יעילה אלא גם מודעת לפרטיות, עם היכולת להטשטש נתונים פרטיים בעת רישום. זה מקטלג לוגים לרמות fault, error, info, ו-debug, כל אחת מספקת דרגת דיוק שונה לצורך פתרון בעיות.

## ראה גם
- [תיעוד הרישום המאוחד של Apple](https://developer.apple.com/documentation/os/logging)
- [מדריך לרישום לוגים של Ray Wenderlich](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [מאגר GitHub של CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)
