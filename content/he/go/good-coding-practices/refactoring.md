---
aliases:
- /he/go/refactoring/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:11.066952-07:00
description: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D1\u05EA\
  \u05DB\u05E0\u05D5\u05EA \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA \u05DE\u05D1\u05E0\
  \u05D4 \u05D4\u05E7\u05D5\u05D3 \u05D4\u05E7\u05D9\u05D9\u05DD \u05DE\u05D7\u05D3\
  \u05E9 - \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05D2\u05D5\u05E8\u05DE\u05D9\u05DD\
  \ - \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05D4\u05EA\
  \u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E0\u05D5\u05E7\u05D8\u05D9\u05DD\
  \ \u05D1\u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E9\
  \u05E4\u05E8 \u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D4\u05E7\u05D5\
  \u05D3, \u05DC\u05D4\u05E7\u05D8\u05D9\u05DF\u2026"
lastmod: 2024-02-18 23:08:52.349681
model: gpt-4-0125-preview
summary: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D1\u05EA\
  \u05DB\u05E0\u05D5\u05EA \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA \u05DE\u05D1\u05E0\
  \u05D4 \u05D4\u05E7\u05D5\u05D3 \u05D4\u05E7\u05D9\u05D9\u05DD \u05DE\u05D7\u05D3\
  \u05E9 - \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05D2\u05D5\u05E8\u05DE\u05D9\u05DD\
  \ - \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA \u05D4\u05EA\
  \u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E0\u05D5\u05E7\u05D8\u05D9\u05DD\
  \ \u05D1\u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E9\
  \u05E4\u05E8 \u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D4\u05E7\u05D5\
  \u05D3, \u05DC\u05D4\u05E7\u05D8\u05D9\u05DF\u2026"
title: "\u05E8\u05E4\u05D0\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
---

{{< edit_this_page >}}

## מה ולמה?

ריפקטורינג בתכנות כולל את מבנה הקוד הקיים מחדש - שינוי הגורמים - מבלי לשנות את התנהגותו החיצונית. מתכנתים נוקטים בתהליך זה כדי לשפר את קריאות הקוד, להקטין את המורכבות ולשפר את ניהוליותו, ובסופו של דבר להקל על הבנת התוכנה ושינוייה.

## איך לעשות:

ב-Go, ריפקטורינג יכול לנוע מתיקוני קוד פשוטים עד לשינויים מורכבים יותר. בואו נתחיל בדוגמה בסיסית: פשט פונקציה ראשונית ב-Go לקריאות ויעילות טובה יותר.

**לפני הריפקטורינג:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    var total float64
    if quantity > 0 {
        total = float64(quantity) * price
    } else {
        total = 0
    }
    return total
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // פלט: 59.9
}
```

**אחרי הריפקטורינג:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    if quantity > 0 {
        return float64(quantity) * price
    }
    return 0
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // פלט: 59.9
}
```

בגרסה שעברה ריפקטורינג, ה-'else' הוסר, דבר שמפשט את זרימת הפונקציה מבלי להשפיע על הפלט שלה - דוגמה לטכניקת ריפקטורינג בסיסית אך בעלת השפעה ב-Go.

לדוגמה מתקדמת יותר, שקלו לעשות ריפקטורינג לפונקציות כדי להשתמש בממשקים לשיפור הניתן לשימוש מחדש והניתן לבדיקה:

**לפני הריפקטורינג:**

```go
package main

import "fmt"

type Logger struct{}

func (l Logger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // דמיינו עיבוד נתונים כאן
    logger.Log("Data processed")
}

func main() {
    logger := Logger{}
    ProcessData("example data", logger)
}
```

**אחרי הריפקטורינג:**

```go
package main

import "fmt"

type Logger interface {
    Log(message string)
}

type ConsoleLogger struct{}

func (c ConsoleLogger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // עיבוד הנתונים נותר ללא שינוי
    logger.Log("Data processed")
}

func main() {
    logger := ConsoleLogger{}
    ProcessData("example data", logger)
}
```

כאשר מבצעים ריפקטורינג לשימוש בממשק (`Logger`) במקום בסוג קונקרטי (`ConsoleLogger`), הפונקציה משתפרת בגמישות והעיבוד נתונים מנותק מהיישום הלוגי הספציפי.

## ניתוח עמוק

ריפקטורינג ב-Go חייב לאזן בין פשטות (אחת מפילוסופיות הליבה של Go) לבין הגמישות הדרושה בפרויקטים גדולים של תוכנה. בהינתן גישת Go המינימליסטית לתכונות - ללא ג'נריקים (עד לאחרונה) ועם דגש חזק על קריאות - השפה מדריכה את המפתחים לעבר מבני קוד פשוטים ונתונים יותר לתחזוקה. עם זאת, זה לא אומר שקוד Go לא מרוויח מריפקטורינג; זה אומר שהריפקטורינג חייב תמיד לתת עדיפות לבהירות ולפשטות.

בהיסטוריה, החוסר של Go בתכונות מסוימות (למשל, ג'נריקים לפני Go 1.18) הוביל לפתרונות יצירתיים אך לעיתים קרובות מסובכים לשימוש חוזר בקוד ולגמישות, מה שהפך את הריפקטורינג להפשטה לנהוג נפוץ. עם הצגת הג'נריקים ב-Go 1.18, מפתחי Go כעת מבצעים ריפקטורינג לקוד הירושה כדי לנצל תכונה זו לבטיחות סוגים טובה יותר ושימוש חוזר בקוד, מה שמדגים את האופי המתפתח של נהלי הריפקטורינג ב-Go.

עם זאת, ערכת הכלים של Go, כולל `gofmt` לעיצוב קוד ו-`go vet` לזיהוי בניות חשודות, תומכת בשמירה על בסיסי קוד נקיים, מה שמפחית את הצורך בריפקטורינג נרחב. בעוד שריפקטורינג הוא כלי יקר ערך בארסנל של מתכנתי Go, שימוש חכם בתכונות ובכלים של השפה מלכתחילה יכול לעזור למזער את הצורך בריפקטורינג מורכב מאוחר יותר.
