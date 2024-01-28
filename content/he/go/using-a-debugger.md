---
title:                "שימוש במנפה שגיאות"
date:                  2024-01-26T03:49:51.119977-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במנפה שגיאות"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש במנפה הוא כמו להחזיק GPS בג'ונגל של קוד; הוא מדריך אותך למקור הבעיה. תכנתים משתמשים במנפים כדי לעבור צעד צעד דרך הקוד שלהם, לבדוק משתנים ולהבין את הזרימה, מה שמקל על תפיסת באגים ושיפור הביצועים.

## איך ל:
ל-Go יש כלי מובנה לניפוי באגים בשם Delve (`dlv`). להתחלה, יש להתקין את Delve, לכתוב תכנית Go פשוטה, ולאחר מכן להריצה דרך המנפה.

```Go
// תחילה, התקן את Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// תכנית Go לדוגמה, שמורה כ-main.go
package main

import "fmt"

func main() {
    message := "ניפוי באגים עם Delve!"
    fmt.Println(message)
}

// הרץ את התכנית שלך עם Delve
// dlv debug

// כמה פקודות בסיסיות של Delve:
// (dlv) break main.main // הגדר נקודת עצירה בפונקציה main
// (dlv) continue // הרץ עד לנקודת העצירה או סיום התכנית
// (dlv) step // צעד צעד באופן בודד דרך התכנית
// (dlv) print message // הדפס את הערך הנוכחי של המשתנה 'message'
// (dlv) quit // צא מ-Delve
```

הפעלת `dlv debug` מתחילה מושב ניפוי באגים. כאשר מגיעים לנקודת עצירה שהוגדרה, ניתן לעבור צעד צעד בתכנית ולראות מה קורה במנוע.

## צלילה עמוקה
בהיסטוריה, תכנתים של Go השתמשו במספר כלים לניפוי באגים כמו GDB (GNU Debugger) אך נתקלו באתגרים מכיוון ש-GDB לא הותאם לזמן ריצה ול-goroutines של Go. Delve הציל את המצב בעל תמיכה טובה יותר בתכונות הייחודיות של Go.

ישנם חלופות ל-Delve כמו `go-dbg`, וכן תמיכה משולבת בניפוי באגים בסביבות פיתוח כמו Visual Studio Code ו-GoLand, המשתמשים ב-Delve לחוויה ידידותית יותר למשתמש.

מבחינת היישום, Delve פועל באמצעות החבילות `runtime` ו-`debug/gosym`, בין השאר, לגישה ופרשנות של סמלים ומידע בזמן ריצה של תכניות Go. הוא מתעדכן באופן קבוע כדי להתאים לתכונות ולגרסאות חדשות של השפה.

## ראה גם
- הרפוזיטורי הרשמי של Delve: https://github.com/go-delve/delve
- מדריך הניפוי באגים של צוות Go: https://golang.org/doc/gdb
- ניפוי באגים ב-Go ב-Visual Studio Code: https://code.visualstudio.com/docs/languages/go#_debugging
