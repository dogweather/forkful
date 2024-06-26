---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:23.922747-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D1\u05E9\u05E4\u05EA Go, \u05E1\u05D5\
  \u05D2 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD `string` \u05D4\u05D5\u05D0 \u05D7\
  \u05EA\u05D9\u05DB\u05D4 \u05DC\u05E7\u05E8\u05D9\u05D0\u05D4 \u05D1\u05DC\u05D1\
  \u05D3 \u05E9\u05DC \u05D1\u05EA\u05D9\u05DD. \u05DC\u05D7\u05D9\u05DC\u05D5\u05E5\
  \ \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1\u05E2\u05D9\u05E7\u05E8 \u05D1\u05EA\u05D7\u05D1\u05D9\
  \u05E8 \u05D4`slice`, \u05DC\u05E6\u05D3 \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D4 \u05D4\u05DE\u05D5\u05D1\u05E0\u05D9\u05EA `len()` \u05DC\u05D1\u05D3\u05D9\
  \u05E7\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.470004-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05E9\u05E4\u05EA Go, \u05E1\u05D5\u05D2 \u05D4\u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD `string` \u05D4\u05D5\u05D0 \u05D7\u05EA\u05D9\u05DB\u05D4 \u05DC\
  \u05E7\u05E8\u05D9\u05D0\u05D4 \u05D1\u05DC\u05D1\u05D3 \u05E9\u05DC \u05D1\u05EA\
  \u05D9\u05DD."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## איך ל:
בשפת Go, סוג הנתונים `string` הוא חתיכה לקריאה בלבד של בתים. לחילוץ תת-מחרוזות, משתמשים בעיקר בתחביר ה`slice`, לצד הפונקציה המובנית `len()` לבדיקת אורך והחבילה `strings` לפעולות מורכבות יותר. הנה כיצד ניתן לעשות זאת:

### חיתוך בסיסי
```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // מחלץ את "World"
    subStr := str[7:12]
    
    fmt.Println(subStr) // פלט: World
}
```

### שימוש בחבילת `strings`
לחילוץ תת-מחרוזות מתקדם יותר, כמו לחלץ מחרוזות לאחר או לפני מחרוזת ספציפית, ניתן להשתמש בחבילת `strings`.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // מחלץ תת-מחרוזת לאחר "="
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // פלט: John Doe
}
```

חשוב לשים לב שמחרוזות ב-Go מקודדות ב-UTF-8 וחתיכה ישירה של בתים לא תמיד תוצא במחרוזות תקינות אם הן כוללות תווים מרובי בתים. לתמיכה ב-Unicode, נכון להשתמש ב-`range` או בחבילת ה`utf8`.

### טיפול בתווים של Unicode
```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // מוצא תת-מחרוזת תוך כדי שימת דגש על תווים של Unicode
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // פלט: 世界
}
```

## צלילה עמוקה
חילוץ תת-מחרוזות ב-Go הינו פשוט, הודות לתחביר החיתוך והספרייה הסטנדרטית הכוללת שלו. היסטורית, שפות תכנות קודמות הציעו פונקציות או שיטות יותר ישירות לטיפול בפעולות מניפולציה כזו של טקסט. עם זאת, הגישה של Go מדגישה בטיחות ויעילות, במיוחד בזכות המחרוזות הבלתי מתמירות שלה והטיפול המפורש בתווים של Unicode באמצעות רונים.

למרות שחיתוך ישיר מתאפשר ביעילות ביצועית, הוא גם מוריש את המסובכים של טיפול ישיר בתווים של UTF-8. הצגת סוג הנתון `rune` מאפשרת לתכניות Go לטפל בטקסט של Unicode בבטיחות, מה שהופך אותה לאלטרנטיבה חזקה ליישומים בינלאומיים.

בנוסף, מתכנתים שבאים משפות אחרות עשויים להתגעגע לפונקציות או שיטות מניפולציה של מחרוזות ברמה גבוהה יותר הכלולות מראש. עם זאת, החבילות `strings` ו-`bytes` בספריית הסטנדרט של Go מציעות מערכת עשירה של פונקציות שאף על פי שדורשות קצת יותר תבניות לקוד, מספקות אפשרויות חזקות לעיבוד מחרוזות, כולל חילוץ תת-מחרוזות.

במהותה, הבחירות העיצוביות של Go בסביבת טיפול במחרוזות משקפות את מטרותיה לפשטות, ביצועים ובטיחות בהתמודדות עם נתוני טקסט מודרניים ובינלאומיים. למרות שייתכן שדרושה התאמה מסוימת, Go מציעה כלים יעילים ואפקטיביים לטיפול בחילוץ תת-מחרוזות ועוד.
