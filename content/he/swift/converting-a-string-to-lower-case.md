---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Swift: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

{

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא תהליך שמביא לאותיות במחרוזת להיצג באותיות קטנות במקום באותיות גדולות. תהליך זה נקרא גם "הטיית סטייל" והוא יימלא לתפקידים שונים בתכנות. מדוע מתכנתים עושים את זה? בעיקר, כדי לפשט מחרוזות ולשקף באופן יחיד את התוכן שלהם.

## איך לעשות זאת:
ניתן להוריד שפת Swift לאותיות רגילות בעזרת פקודת `lowercased()` על עצמת פרמטר ספציפי כדי לבצע את השינוי הרצוי. לדוגמא:

```Swift
let string = "HELLO WORLD"
let lowercasedString = string.lowercased()
print(lowercasedString)

// Output: hello world
```

ניתן גם להשתמש במתודת `lowercased()` עם `components(separatedBy: .newlines)` כדי להוריד שורה חדשה במחרוזת.

```Swift
let multiLineString = "Hello\nWorld"
let lowercasedString = multiLineString.lowercased()
let lines = lowercasedString.components(separatedBy: .newlines)
print(lines)

// Output: ["hello", "world"] 
```

## חקירה עמוקה:
פונקציות למרות היותן חלק מכיוון מסוים של ג'ווה סקריפט, לא ייחודוֹה וייפורס של הטימה. פורמיה רווח את המודל בעיקרון של "מחרוזות" השואבות מה"ידים הראשונות "של בעיות האבטחות. למשל אלגורטמים הטומנים מידע ייעודיים למתקדמיים יחייך היכולת להביא לכלעמית נתונים עם אותם שיטות.

פתרון אחד אפשרי הוא להשתמש במשתנים בצורה כוללת לחייב פרטיים ספציפיים, ניתן לעבוד את מסתשר בתוך ממש הפתירות הייחודיות. פתרון הפניטו היחדי הוא לדוגמא במתנה לפטור אותיות בספר עם אתרי פשרנות תקציר אלגוריתמי מכתביים בתיקון ספציפי במספר כמו  פרטי למפתח ב שהחלטו להתייחס וו שמציגים של תג " חיזוק נקודות" זה הוא בדם הסופרת מבחינה של האמבוכורחות הזו.

פתרון נוסף הינו להשתמש במתודת `filter` כדי להוריד את האותיות הגדולות מהמחרוזת. למשל:

```Swift
let string = "Hello World"
let filteredString = String(string.filter { !"A"..."Z" ~= $0 })
print(filteredString)

// Output: ello orld
```

## ראה גם:
למידע נוסף על מהו מחרוזת וכיצד להפיק מידע מה מחרוזות בשפת Swift, ניתן לקרוא את המדריך הקצר והמעודכן של אפל על מחרוזות בשפת Swift: https://developer.apple.com/documentation/swift/strings-and-characters. ייתכן גם למצוא את שפת Swift 5.5 בעזרת ניתוחים אחרים על הספרייה הקיימת הכפל שחוששים להשתמש במחזור המבוקש בשפת C: https://swift.org.