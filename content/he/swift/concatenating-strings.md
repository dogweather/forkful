---
title:    "Swift: צירוף מחרוזות"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

כתיבת קוד תכנותי ניתן המשימה של חיבור מחרוזות נראית פשוטה מבחינה טכנית, אבל מהם היתרונות האמיתיים של קישור מחרוזות? נדבר על כמה מבני השפה של Swift והפיתויים שהם מביאים ואת כמה דברים שייעזרו לך לקחת משימה זו בצורה חכמה יותר.

## איך לעשות זאת

תחילה, בואו נתחיל עם פונקציית הפיתרון התדיר ביותר: חיבור של שתי מחרוזות פשוטות. נשתמש בסימן "+" כדי להוסיף את המחרוזות יחד. הנה דוגמה לכתיבת הקוד עם פלט:

```Swift
let firstString = "שלום,"
let secondString = "עולם!"
let resultString = firstString + " " + secondString

print(resultString)
```

פיתרון זה ישתמש באופרטור "+" כדי להוסיף פשוט את המחרוזות ביחד וליצור את המחרוזת החדשה שהמכילה גם את "שלום," וגם את "עולם!"

ביחד עם זאת, אפשר להשתמש גם בפיתרון הקרוב ל-"Increment" לפוינקציות של מחרוזות ביישום של StringInterpolationProtocol. בזמן היישומים, אתה יכול ליצור מחזור ליסט ולכתוב בפיתרון זה מיד את הפונקציה, כמו פה:

```Swift
let firstString = "אני רוצה לתת דוגמה לתוכנית אימון  - Name= \(myNameValue)"
```

בדוגמה זו, אתה מבקש לקחת את המחרוזות "אני רוצה לתת דוגמה לתוכנית אימון - Name=" ו"Name Value" להניח ששמך הוא "khalid" ואתה מנגד קרי בסופרמן הפרתיון שיכול לקריא לשם זה ולחזקו זה יחד (אם יש לך כדי לגמור בלייבל שני):

```Swift
let firstString = "Hello"
let secondString = "Khalid"
let resultString = "\(firstString) \(secondString)"

print(resultString)
```

יכול להיות מוצר ל "שלום כליד" אותו אנחנ