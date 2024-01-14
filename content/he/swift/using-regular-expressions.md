---
title:                "Swift: שימוש בביטויים רגולריים"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

בעולם התכנות, לעיתים קרובות נתקלים במצבים שבהם נדרש למצוא או להתאים מחרוזות מסוימות בטקסט. בדיקת תוכן בטקסט, תיקון אוטומטי של שגיאות וחיפוש לפי פטרנים מסוימים הם דוגמאות למצבים מסוימים שבהם נדרשת השתמשות בביטויים רגולריים. בעזרתם שלהם, אנחנו יכולים לחפש, להתאים ולעבד מחרוזות בקלות ובצורה ביעילה. בכתבה הזו, אנחנו נלמד איך להשתמש בביטויים רגולריים בשפת סוויפט ומדוע הם חשובים כל כך בתכנות.

## How To

להלן נתונים דוגמתיים של כיצד להשתמש בביטויים רגולריים בשפת סוויפט:

```Swift
// ייבוא מודול נדרש עבור ביטויים רגולריים
import Foundation

// יצירת צירוף תווים
let string = "This is a sample string to test regular expressions."

// חיפוש לפי תבנית כלשהי
let pattern = "sample"
let regex = try! NSRegularExpression(pattern: pattern, options: .caseInsensitive)
let match = regex.matches(in: string, options: [], range: NSRange(string.startIndex..., in: string))

// הדפסת תוצאה
print("Found match at index: \(match[0].range.rangeValue.lowerBound)")
```

מה שהקוד מעלה הוא לבדוק האם המילה "sample" מופיעה במחרוזת הנתונה. אנחנו משתמשים בתוצאה של הפונקציה `regex.matches` כדי להאמת את החיפוש ולמצוא את ההתאמה הראשונה במחרוזת. במקרה זה, הפלט יודפס כ "Found match at index: 10", מה שמסמן כי כתיבת המילה "sample" מתחילה בתו העשירי במחרוזת.

## Deep Dive

בנוסף לדוגמאות של חיפוש והתאמה, ביטויים רגולריים מציגים גם את היכולת לשנות מחרוזות ולהסיר תווים מיותרים או להחליף אותם בתווים אחרים. ל