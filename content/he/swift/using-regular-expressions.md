---
title:                "Swift: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה
פעולות לחיפוש והתאמה של פטרנים (regular expressions) הן כלי חזק ופופולרי בתכנות, שמאפשר למצוא ולהתאים חריגים בטקסט קל ומהיר. בתוך כמה שורות לקוד ניתן לבצע מספר פעולות מורכבות על טקסט, כגון חיפוש והחלפה של מחרוזות כלליות, תיקון שגיאות כתיב בקלות ועוד.

## איך להשתמש בפטרנים כלליים ב-Swift
כדי להשתמש בפטרנים כלליים בשפת Swift, ניתן להשתמש במחלקה שלנו שהוקמה על ידי האובייקטาי הנטיבית, `NSRegularExpression`. ניתן להשתמש בפונקציות הלא מורכבות להגדרת אתרים חלקיים או לבדוק התאמה מלאה של פטרנים. לדוגמה:

```Swift
let text = "שלום לכולם! שיר המיסטרי אריק קלפטון"

do {
    let regex = try NSRegularExpression(pattern: "שיר המיסטרי \\S+")
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    for match in matches {
        print("נמצאה התאמה באינדקסים \(match.range)")
    }
} catch {
    print("לא נמצאו התאמות")
}

// prints: נמצאה התאמה באינדקסים {15, 11}
```

כאן, אנו בודקים האם יש מילה שמתחילה ב-"שיר המיסטרי" ולאחריה יש רווח ואז כל תו חוקי. אם תמצא התאמה, אנחנו מדפיסים את האינדקסים של התאמה במחרוזת המקורית.

בנוסף לזה, ניתן להשתמש בפטרנים כלליים גם בפונקציות כמו `component(separatedBy: " ")` כדי להפריד מחרוזות לפי פטרנים מסוימים.

## לעומק
פטרנים כלליים הם כלי עוצמתי וחשוב לתכנות. כאשר משתמשים בהם בצורה נכונה, ניתן לחסוך הרבה זמן