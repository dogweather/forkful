---
title:                "הופעלות כתבת"
html_title:           "Swift: הופעלות כתבת"
simple_title:         "הופעלות כתבת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

כל מתכנת ידעות על קפיטליזציה תהיה יכולה להגיב על כך שזה פשוט, ולמה לועזים מדברים כל הזמן על זה? אבל האמת היא שהפעולה הזו יכולה להיות פתרון מושלם לבעיות של כאלו שמנסים להדפיס טקסט צריך להיראות כמו כותרת, או לכתוב להודית שיש לנו פונקציה עם אינפוט שהתוצאה שלה תהיה צריכה להיות מאותו הצורה.

## למה

לכיוון שקפיטליזציה היא הפעולה שמסדרת את האותיות מפתח האות הראשון לעומת אינדקסים נמוכים יותר, אנו מייצרים מערכת שהתוצאה שלה תהיה בצורה אופטימלית. זה נותן לנו את היכולת ליצור קוד פשוט הרבה יותר, וזה גם נותן לנו יכולת לראות את התוצאה שלנו בפורמט שנרצה.

## איך לעשות את זה

כפי שצוין מעלה, קפיטליזציה היא פעולה שמייצרת תוצאה צורתית שלא מתבססת על כמה אופי ואינדקסים - היא תמיד תשתמש במונח אחד לשם התהליך של הנקודה הראשונה של הטקסט. להלן כמה דוגמאות לקוד Swift עבור קפיטליזציה:

```Swift
let myString = "hello world"
print(myString.capitalized)
// Output: Hello World
```

```Swift
let myString = "this is a test"
print(myString.uppercased())
// Output: THIS IS A TEST
```

```Swift
let myString = "alphaBetaGamma"
print(myString.lowercased().capitalized)
// Output: Alphabetagamma
```

כפי שאתם רואים, ישנן שתי אפשרויות עבור קפיטליזציה - `capitalized` ו- `uppercased()`, אך השתמשנו גם בפעולת `lowercased()` כדי להפעיל את המתודה הרלוונטית (במקרה זה `capitalized`) רק על האותיות הטקסט היותר אותיות.

## העומ