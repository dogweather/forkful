---
title:                "הגדלת אותיות במחרוזת"
date:                  2024-02-03T19:07:21.960514-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

הקפיטליזציה של מחרוזת ב-Swift מעדכנת את המחרוזת כך שהתו הראשון שלה הופך לאות גדולה, ושאר התווים הופכים לאותיות קטנות. מתכנתים עושים זאת למטרות כמו עיצוב שמות או משפטים על פי כללי דקדוק או סטנדרטים של ממשק משתמש.

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
