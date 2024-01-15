---
title:                "חיפוש והחלפת טקסט"
html_title:           "Swift: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מדוע

כתיבת תוכניות אינה דבר קל, ולפעמים אנו נתקלים בצורך לשנות טקסט בתוך הקוד. במקום להעתיק ולהדביק שוב ושוב, ניתן להשתמש בפעולת חיפוש והחלפה על מנת לכפול קטעי קוד ולשנות אותם בקלות.

## כיצד לעשות זאת

### חיפוש טקסט והחלפה

על מנת לחפש ולהחליף טקסט בקוד שלנו, ניתן להשתמש בפונקציות `replacingOccurrences(of:with:)` או `replacingOccurrences(of:with:options:range:) ` שמציעות לנו פרמטרים נוספים לשלוט במקומות הספציפיים בהם אנחנו מחפשים ומחליפים טקסט.

אלו הם הפרמטרים של הפונקציה `replacingOccurrences`:

- `of`: טקסט המחליף אותו אנחנו מחפשים.
- `with`: טקסט המחליף את הטקסט המצוי במקום החיפוש.
- `options`: אפשרויות נוספות לחיפוש והחלפה, למשל שינוי רישות הכתיב והחיפוש מתחת לגדלות.
- `range`: טווח של מיקומים מסוימים בטקסט שאנחנו מחפשים ומחליפים.

בהמשך נלמד כיצד להשתמש בפרמטרים הנוספים כדי לשלוט בפעולת החיפוש והחלפה.

נציג כמה דוגמאות של כיצד ניתן להשתמש בפונקציות חיפוש והחלפה:

```
// דוגמה 1: החלפת טקסט בכל הטקסט
let originalText = "Hello code lovers! This is a sample text."
let replacedText = originalText.replacingOccurrences(of: "code", with: "Swift")
print(replacedText)
// Output: "Hello Swift lovers! This is a sample text."

// דוגמה 2: החלפת טקסט רק בטקסט מסוים
let quote = "I love coding with Swift!"
let replacedQuote = quote.replacingOccurrences(of: "coding", with: "programming", options: .caseInsensitive)
print(replacedQuote)
// Output: "I love programming with Swift!"

// דוגמה 3: החלפת טקסט בת