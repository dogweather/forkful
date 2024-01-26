---
title:                "הפיכת מחרוזת לאותיות רישיות"
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הקפלה לראשי תיבות של מחרוזת משנה את כל האותיות קטנות לגדולות. תכנתים עושים זאת לנוחות, כדי להקל על קריאת טקסט וכדי להבטיח התאמה למוסכמות קוד (כגון קבועים).

## איך לעשות:
```Swift
let lowerCaseString = "shalom, swift!"
let capitalizedString = lowerCaseString.uppercased()
print(capitalizedString) // Outputs: "SHALOM, SWIFT!"
```
השימוש בפונקציה `uppercased()` של Swift הופך את כל האותיות במחרוזת לראשי תיבות.

## עיון נוסף:
בעבר, קפליזציה אולי יכלה להשתנות בין מערכות שונות בגלל קידודי תווים ייחודיים. היום, Swift משתמשת ב-Unicode, מה שמאפשר התנהגות עקבית בכל פלטפורמה. ראו חלופות כמו `.lowercased()` להקטנת אותיות, או `.capitalized` שרק האות הראשונה של כל מילה הופכת לראשית.

## ראו גם:
- תיעוד Swift של Apple על מחרוזות ותווים: https://developer.apple.com/documentation/swift/string
- Unicode Standard: https://unicode.org/standard/standard.html
- קהילת מפתחי Swift: https://swift.org/community/
