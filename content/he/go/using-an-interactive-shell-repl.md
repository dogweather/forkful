---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:14:48.483365-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
REPL (לולאת קריאה-חישוב-הדפסה) מאפשרת אינטראקציה חיה עם קוד; היא קוראת קלט, מעריכה אותו, מדפיסה את התוצאה, ומחזירה ללולאה. תוכניתנים משתמשים בה כדי לבדוק קטעי קוד, לאבחן תקלות, וללמוד שפות חדשות בזמן אמת.

## איך ל:
Go לא כוללת ממשק REPL מובנה, אך ניתן להשתמש בכלים של צד שלישי. אחד הכלים הפופולריים הוא `gore`:

```go
// התקנת gore באמצעות
$ go install github.com/motemen/gore/cmd/gore@latest

// הרצת gore
$ gore
gore version 0.5.0  :help for help
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## צלילה עמוקה
מקורית פותחו REPLs עבור Lisp, והן נפוצות בשפות דינמיות כמו Python או Ruby. Go, המסווגת כשפה עם טיפוסיות סטטית, לא כוללת אחת מובנית. חלופות ל-`gore` כוללות את `go-pry` ו-`yaegi`. כלים אלו מפרשים קוד Go, מאפשרים לך לחקור ולאמת רעיונות במהירות מבלי להדר יישום מלא. הם במיוחד שימושיים למתחילים ובהקשרים חינוכיים שבהם הדגש הוא על למידה וניסוי.

## ראה גם
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry)
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
