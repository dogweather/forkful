---
title:                "Swift: מציאת אורך של מחרוזת."
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה

כשמתעסקים בתכנות ב-Swift, יש כמה דברים בסיסיים שעלינו ללמוד. אחד מהם הוא אורך של מחרוזת (string). אורך של מחרוזת הוא מספר התווים במחרוזת, והיא חלק חשוב במחלקות מחרוזות ובפונקציות. במאמר הזה, נלמד איך למצוא את אורך המחרוזת ב-Swift ונכיר בעמק יותר במושג זה.

## כיצד למצוא את אורך המחרוזת

כדי למצוא את אורך המחרוזת ב-Swift, נשתמש בפונקציה `count` שמובנית במחלקת `String`. ניתן להשתמש בפונקציה זו על מחרוזת כלשהי כדי לקבל את מספר התווים בה. ניתן לראות את השימוש בפונקציה `count` בקטע הקוד הבא:

```Swift
let string = "זהו דוגמה למחרוזת."
print(string.count)
```

פלט: `19`

## מכל הצדיקים

רוב המתכנתים מכירים את הפונקציה `count` לצורך חישוב אורך המחרוזת. אבל מה בדיוק קורה מאחורי הקלעים? איך הפונקציה יודעת לקבוע את אורך המחרוזת?

כאשר נקרא לפונקציה `count`, המחרוזת נשלחת לפונקציה שלב אחד בתוך המחסנית (stack frame) של הקוד. פונקציה זו יוצרת אובייקט מסוג `String.Index`, שכולל את מיקום התו הראשון במחרוזת, ואת מיקום התו האחרון. המיקום האחרון משמש כנקודת יציאה מתוכנית הקוד ומכאן היא מחזירה את האורך של המחרוזת כתוצאה.

## ראו גם

- [String - Swift docs](https://developer.apple.com/documentation/swift/string/1642994-count)
- [רפרנס של מחלקת String בחבילת הכווץ של אפל](https://github.com/apple/swift/blob/master/stdlib/public/core/StringIndex.swift)