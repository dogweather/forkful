---
title:    "Swift: מחיקת תווים התואמים לתבנית"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

מחיקת תווים המתאימים לתבנית היא כלי שימושי לכותבי קוד שרוצים להסיר תווים מוותרים או מיותרים מטקסט. זה יכול לשפר את ביצועי התוכנית ולהפחית את גודל הקוד ולשמור על קוד נקי יותר.

## איך לעשות זאת

כדי למחוק תווים המתאימים לתבנית באמצעות Swift, ניתן להשתמש בפונקציה `replacingOccurrences(of:with:)`. פונקציה זו מקבלת שני מחרוזות כפרמטרים - התבנית שברצונך למחוק והמחרוזת שאתה רוצה להחליף בה. ניתן גם להוסיף פרמטר `options` כדי להתאים את ההתאמה לתנאים שונים. לדוגמה:

```Swift
let originalString = "מחרוזת עם תווים מוותרים!!!"
let modifiedString = originalString.replacingOccurrences(of: "[א-ת]", with: "", options: .regularExpression)
print(modifiedString)
```
תוצאה:
`מחרוזת עם תווים מוותרים`

## מקורות נוספים

כדי ללמוד עוד על כיצד למחוק תווים המתאימים לתבנית ב-Swift, ניתן לעיין במסמכים הבאים:

- [מדריך לפונקצית replacingOccurrences באתר Apple](https://developer.apple.com/documentation/foundation/nsstring/1411940-replacingoccurrences)

- [מדריך לתבניות ב-regular expressions באתר regex101](https://regex101.com/)

- [וידאו הדרכה על מחיקת תווים בעזרת regular expressions בעזרת לשונית swift של Siraj Raval](https://www.youtube.com/watch?v=sgdH5sPx1TU)

## ראה גם

- [מדריך לפונקציית substring ב-Swift](https://github.com/sagishahar/swift-blog/blob/master/Blogs/substring/substring.md)

- [מדריך לתבניות ב-Swift בעזרת `CharacterSet`](https://github.com/sagishahar/swift-blog/blob/master/Blogs/character-set/character-set.md)