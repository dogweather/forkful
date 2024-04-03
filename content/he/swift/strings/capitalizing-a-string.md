---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:21.960514-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DE\u05D1\u05E0\
  \u05D9 `String` \u05D1-Swift \u05DE\u05D2\u05D9\u05E2\u05D9\u05DD \u05E2\u05DD \u05DE\
  \u05E1\u05E4\u05E8 \u05E9\u05D9\u05D8\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D5\
  \u05EA \u05DC\u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05D2\u05D5\u05D3\u05DC \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05D4\u05E0\u05D4 \u05DB\u05DE\
  \u05D4 \u05D3\u05E8\u05DB\u05D9\u05DD \u05DC\u05D4\u05E7\u05E4\u05D9\u05E5 \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1-Swift, \u05DB\u05D5\u05DC\u05DC \u05D4\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E9\u05D9\u05D8\u05D5\u05EA \u05E1\u05D8\
  \u05E0\u05D3\u05E8\u05D8\u05D9\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.878111-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05D1\u05E0\u05D9 `String` \u05D1-Swift \u05DE\u05D2\u05D9\u05E2\u05D9\
  \u05DD \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8 \u05E9\u05D9\u05D8\u05D5\u05EA \u05DE\
  \u05D5\u05D1\u05E0\u05D5\u05EA \u05DC\u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05D2\
  \u05D5\u05D3\u05DC \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## איך לעשות:
מבני `String` ב-Swift מגיעים עם מספר שיטות מובנות לשינוי הגודל של מחרוזות. הנה כמה דרכים להקפיץ מחרוזות ב-Swift, כולל השימוש בשיטות סטנדרטיות ובספריות מצד שלישי אם נחוץ.

### שימוש בשיטות מובנות
להקפיץ את האות הראשונה של מחרוזת ולהקטין את שאר האותיות:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // פלט: "Hello, world"
```

להקפיץ את האות הראשונה של כל מילה במשפט, אפשר להשתמש בתכונה `capitalized`:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // פלט: "Hello, World"
```

### שימוש בספרייה מצד שלישי
למרות שספריית הסטנדרט של Swift היא די מקיפה, ייתכן שפורמטים מסוימים של הקפיטליזציה דורשים מבצעים מורכבים יותר או שניתן לפשטם באמצעות ספריות מצד שלישי. אחת מהספריות הפופולריות לניפוי מחרוזות היא SwiftRichString. (הערה: תמיד וודאו לכלול ספריות מצד שלישי דרך Swift Package Manager, CocoaPods, או Carthage, ולייבא אותם בקובץ שלכם.)

ראשית, יהיה צורך להוסיף את `SwiftRichString` לפרויקט שלכם. לאחר התקנתה, תוכלו להשתמש בה לבצע מגוון פעולות עם מחרוזות, כולל צרכים ספציפיים של הקפיטליזציה. עם זאת, כרגע, שיטות המובנות של Swift מכסות באופן נאות את רוב צרכי ההקפיטליזציה ללא הצורך בספריות חיצוניות רק לצורך הקפצת מחרוזות.

תמיד התייחסו לתיעוד העדכני ביותר של הספרייה לכל עדכון או שינוי בשיטות.
