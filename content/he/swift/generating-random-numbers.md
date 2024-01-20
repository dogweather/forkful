---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים היא בסך הכל שיטה שבה מספרים מוחלטית או חלקית אקראיים מופקים. מתכנתים משתמשים בה שכן זו דרך להפוך את התוכנה לאבדיקטיבית, למשל במשחקים או בעת ביצוע בדיקות סטרס.

## איך לעשות:
בהנחה שאתה כבר מכיר את שפת Swift, דרך פשוטה ליצירת מספר אקראי היא להשתמש בפונקציה `Int.random`.

```Swift
let randomNumber = Int.random(in: 1..<10)
print(randomNumber)
```
במחבלול, כאשר ריצת הקוד, הוא יחזיר מספר בין 1 ל-10.

## בעומק:
האמת היא שמהלך זה הוא לא אקראיות אמיתית, אלא מה שנקרא "אקראיות דמותית". זו כביכול אקראיות, וברוב המקרים זה מספיק.
אבל, אם אתה מחפש אקראיות אמיתית (למשל לצורכים קריפטוגרפיים), עליך לחפש באפשרויות אחרות.
היסטורית, שימוש במחוללים אקראיים יעילים ומבינים היה מורכב, אך Swift כבר מתמודדת עם זאת באופן אלגנטי.
מחוללי המספרים האקראיים של Swift מבוססים על האלגוריתם Xorshift.

## ראו גם:
- [Apple Documentation on Random](https://developer.apple.com/documentation/swift/random/)
- [Wikipedia article on Pseudorandomness](https://he.wikipedia.org/wiki/%D7%90%D7%A7%D7%A8%D7%90%D7%99%D7%95%D7%AA_%D7%93%D7%9E%D7%95%D7%AA%D7%99%D7%AA)
- [SwiftRocks Tutorial on Swift's Random API](https://swiftrocks.com/an-in-depth-look-at-swifts-random-number-api.html)