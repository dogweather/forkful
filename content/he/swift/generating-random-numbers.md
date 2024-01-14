---
title:                "Swift: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

ברוכים הבאים לכתבה השבועית שלנו על תכנות בשפת Swift! השבוע נביט במחולל מספרים אקראיים ונלמד כיצד ליצור מספרים אקראיים בקוד שלנו. כתבתי את המדריך הזה עבור קוראי העברית שמעוניינים ללמוד כיצד ליצור מספרים אקראיים בשפת Swift.

## למה

למה אנו בכלל מתעניינים ביצירת מספרים אקראיים? למדעי המחשב יש הרבה שימושים למספרים אקראיים, כגון מבחני תוכנה, משחקי מחשב ואלגוריתמים שונים. לבנות מחולל מספרים אקראיים בשפת Swift הוא כלי חיוני בכדי ליצור יישומים מעניינים ומרתקים.

## כיצד לעשות זאת

תחילה נגדיר פונקציה פשוטה שתקבל מספר מקסימלי ותחזיר מספר אקראי בין 0 למספר המקסימלי כפי שמופיע בקוד הבא:

```Swift
func randomInt(maximum: Int) -> Int {
    return Int(arc4random_uniform(UInt32(maximum)))
}
```

ניתן לקרוא לפונקציה הזאת על ידי הזנת מספר המקסימלי כמשתנה, כמו שנמדוד בקוד הבא:

```Swift
let randomNumber = randomInt(maximum: 10)
print(randomNumber) // Output: תראו מספר אקראי בין 0 ל-10
```

כעת, נראה דוגמא נוספת שבה נציג כיצד ליצור מערך של מספרים אקראיים בעזרת לולאת "for", כפי שנראה בקוד הבא:

```Swift
var randomNumbers: [Int] = []
for _ in 1...5 {
    randomNumbers.append(randomInt(maximum: 20))
}
print(randomNumbers) // Output: תראו את המערך של המספרים האקראיים
```

## לחפור עוד עמוק

ניתן להשתמש בפונקציה המובנית של שפת Swift "arc4random_uniform" כדי ליצור מספרים אקראיים. בנוסף, ניתן להשתמש בספריית "GameplayKit"