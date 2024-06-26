---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:30.142461-07:00
description: "\u05D0\u05D9\u05DA \u05DC: Go \u05DE\u05E1\u05E4\u05E7\u05EA \u05DE\u05EA\
  \u05E7\u05DF \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DC\u05E0\u05D9\u05E4\u05D5\u05D9\
  \ \u05EA\u05E7\u05DC\u05D5\u05EA \u05D1\u05E9\u05DD `delve`. \u05D6\u05D4\u05D5\
  \ \u05DB\u05DC\u05D9 \u05DC\u05E0\u05D9\u05E4\u05D5\u05D9 \u05EA\u05E7\u05DC\u05D5\
  \u05EA \u05DE\u05DC\u05D0 \u05EA\u05DB\u05D5\u05E0\u05D5\u05EA \u05E9\u05DE\u05D0\
  \u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05D1\u05E6\u05E2 \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05D5\u05EA Go \u05E6\u05E2\u05D3 \u05D0\u05D7\u05E8 \u05E6\u05E2\u05D3,\
  \ \u05DC\u05D1\u05D3\u05D5\u05E7 \u05DE\u05E9\u05EA\u05E0\u05D9 \u05EA\u05D5\u05DB\
  \u05E0\u05D9\u05EA, \u05D5\u05DC\u05D4\u05E2\u05E8\u05D9\u05DA\u2026"
lastmod: '2024-03-13T22:44:38.498379-06:00'
model: gpt-4-0125-preview
summary: "Go \u05DE\u05E1\u05E4\u05E7\u05EA \u05DE\u05EA\u05E7\u05DF \u05DE\u05D5\u05D1\
  \u05E0\u05D4 \u05DC\u05E0\u05D9\u05E4\u05D5\u05D9 \u05EA\u05E7\u05DC\u05D5\u05EA\
  \ \u05D1\u05E9\u05DD `delve`."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05EA\u05D7 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
weight: 35
---

## איך ל:
Go מספקת מתקן מובנה לניפוי תקלות בשם `delve`. זהו כלי לניפוי תקלות מלא תכונות שמאפשר לך לבצע תוכניות Go צעד אחר צעד, לבדוק משתני תוכנית, ולהעריך ביטויים.

להתחיל, עליך להתקין את `delve` קודם. תוכל לעשות זאת על ידי הפעלה של:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

עכשיו, בואו ננפה תקלות בתוכנית Go פשוטה. בחרו בתוכנית `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

להתחיל בניפוי תקלות של התוכנית הזאת, פתחו טרמינל בספריית הפרויקט והריצו:

```shell
dlv debug
```

פקודה זו מקומפלת את התוכנית עם ביטול אופטימיזציות (כדי לשפר את חוויית הניפוי), מתחילה אותה, ומחברת לה מנפה תקלות.

כאשר `delve` רץ, אתם נמצאים בקליפת המנפה האינטראקטיבית. הנה מספר פקודות בסיסיות:

- `break main.main` מגדיר נקודת עצירה בפונקציה `main`.
- `continue` ממשיך בהרצת התוכנית עד שנפגעת נקודת עצירה.
- `print message` ידפיס את ערך המשתנה `message`.
- `next` מתקדם בהרצת התוכנית לשורה הבאה.
- `quit` יוצא מהמנפה.

הפלט כאשר מגיעים לנקודת העצירה ומדפיסים את המשתנה עשוי להיראות כך:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debugging in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debugging in Go"
```

באמצעות הפקודות האלה, אתם יכולים ללכת דרך התוכנית שלכם, לבדוק את המצב בזמן שאתם הולכים כדי להבין איך היא מתנהלת, ולזהות בעיות כלשהן.

## חקירה עמוקה
הבחירה ב-`delve` ככלי לניפוי תקלות מועדף על כלים מסורתיים כמו GDB (GNU Debugger) נובעת בעיקר מטבעה של מודל הביצוע והריצה של Go. GDB לא תוכנן במקור עם הרצת Go בראש, דבר שהופך את `delve` לבחירה מתאימה יותר למפתחי Go. `Delve` מתוכנן במיוחד עבור Go, ומציע חוויית ניפוי תקלות יותר אינטואיטיבית עבור גורוטינות של Go, ערוצים, וקונסטרוקטים נפוצים אחרים ב-Go.

בנוסף, `delve` תומך במגוון רחב של תכונות מעבר לאלה שמציע GDB בסיסי בעבודה עם תוכניות Go. כולל, אבל לא מוגבל ל: התמקדות בתהליכים רצים לצורך ניפוי תקלות; נקודות עצירה מותנות; והערכת ביטויים מורכבים שעשויים לכלול את אבני הבניין המתקדמות של המקביליות של Go.

למרות ש-`delve` הוא המנפה המועדף על רבים ממפתחי Go, כדאי לציין שכלי ה-GO כולל גם תמיכה קלה יותר בניפוי תקלות, כמו כלי ה-`pprof` המובנה לפרופילינג וכלי ה-`trace` להמחשת מקביליות. כלים אלה יכולים לפעמים לספק דרך מהירה יותר או ברמה גבוהה יותר לאבחון בעיות ביצועים או באגים שקשורים למקביליות, שעשויים להיות משלימים או אפילו מועדפים בהתאם להקשר הניפוי.
