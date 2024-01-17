---
title:                "עבודה עם קבצי CSV"
html_title:           "Swift: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-csv.md"
---

{{< edit_this_page >}}

שלום וברוכים הבאים למדריך מהיר לתכנות בשפת Swift! הפעם אנחנו נדבר על עבודה עם קבצי CSV ואיך לעזור לכם כמתכנתים בפעולה זו. בואו נתחיל!

## מה ומדוע?

CSV היא ראשי תיבות של Comma Separated Values, שמשמעותו היא ערך שמופרד בפסיק. זהו פורמט נפוץ לאחסון ולשיתוף נתונים בקבצים טקסט. מתכנתים משתמשים בקבצי CSV כדי לשמור נתונים כגון ספרות, מחרוזות ותאריכים בצורה קלה ונוחה.

## איך לעשות זאת?

דוגמאות קוד ופלט מופשט:
```Swift
// ייבוא הספרייה Foundation
import Foundation

// שימוש בפונקציה "components(separatedBy:)" כדי לספק את הנתונים מהנתיב של הקובץ
if let csvFile = Bundle.main.url(forResource: "file", withExtension: "csv"),
    let csvString = try? String(contentsOf: csvFile) {
        let csvDataString = csvString.components(separatedBy: ",") // הפרדת נתונים כפי שמוגדר בספרייה
        print(csvDataString) // פלט: ["שלום", "היי", "אני", "כותב", "מדריך"]
}
```

## מעמקים נמוכים

הפורמט CSV נוצר כבר בשנות ה־70 כדי לקלוט נתונים רבים בצורה קלה ובפשטות. במקור, ניתן היה לשמור נתונים רק שפריצות תרומת, אך בשנים האחרונות הפורמט התפתח וכיום ניתן לשמור בו גם מידע רב יותר תובנתי כמו תאריכים ותוויות.

למרבה המזל, ישנם אלטרנטיבות לפורמט CSV כמו JSON ו־XML שמציעות יכולות מתקדמות יותר ויישומים רבים יותר. אך CSV נשאר פופולרי מכיוון שהוא נוח ופשוט לשימוש.

ב־Swift, ניתן להשתמש במספר ספריות כגון Foundation ו־CSVParser כדי לעבוד עם קבצי CSV. המימוש תלוי בכיצד בוחרים לעבוד עם הנתונים ואם ישנם ספריות נוספות שמציעות תכונות לעיבוד נתוני CSV בצורה מתקדמת יותר.

## ראו גם

למידע נוסף ודוגמאות קוד נוספות, אתם מוזמנים לצפות בפרויקט "CSV Parser" בפורום המסומך של שפת Swift.

כמו כן, ניתן למצוא מידע נוסף על פורמט CSV ואת ההסטוריה שלו באמצעות חיפוש באינטרנט או נתוני לקוחות חיצוניים.