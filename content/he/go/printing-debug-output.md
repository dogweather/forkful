---
title:                "הדפסת פלט ניתוח שגיאות"
html_title:           "Go: הדפסת פלט ניתוח שגיאות"
simple_title:         "הדפסת פלט ניתוח שגיאות"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלטי ניפוי (debug output) היא כלי חשוב בתהליך הפיתוח. זהו התהליך שבו מפתחים מנסים לחשוף שגיאות ובאגים בקוד כדי לתקן אותם. דרך כזו של ניפוי אינה פשוטה ולכן מתייחסים לה כדי להקל על התפקיד.

## איך לעשות?
לעיתים קרובות המנפיקים של Go משתמשים ב```fmt.Println``` כדי להדפיס פלטי ניפוי בקונסול כאשר קוד נמצא בשלב הפיתוח. ניתן לראות את הפלט המדגים בדוגמה הבאה:

```
Go fmt.Println("Hello world!")
// Output: Hello world!
```

אם יש צורך להדפיס ערכים משתנים או משתנים מיכסים, ניתן לעשות זאת בעזרת הפונקציות הרבות שכבר קיימות בגופן:

```
Go fmt.Printf("My name is %s and I am %d years old", name, age)
// Output: My name is John and I am 25 years old
```

ניתן גם לבדוק את התווית של משתנים כדי לדעת מתי התוכנית משתמשת בהם בצורה לא נכונה:

```
Go fmt.Println("myVariable:", myVariable)
// Output: myVariable: 5
```

וכן ניתן להדפיס מספר שורות על ידי הפעלת מספר פעמים של הפקודה ```fmt.Println```:

```
Go fmt.Println("Line 1")
Go fmt.Println("Line 2")
Go fmt.Println("Line 3")

// Output:
// Line 1
// Line 2
// Line 3
```

## העמקה
הדפסת פלטי ניפוי היא כלי נפוץ ושימושי בתהליך הפיתוח. זהו כלי שנמצא כבר מלפני שנים רבות ועוד מתחדש עם הטכנולוגיות החדשות. למרבה המזל, ישנם גם כלים נוספים עבור תהליך הניפוי כמו למשל תוכניות ניפוי אוטומטיות (automated debugging), לוגרים (loggers) וסיימום עומעניים (stack traces). כלים אלה יעזרו לך לנתח את השגיאות והבאגים בקוד שלך.

## ראה גם
ללימוד נוסף על ניפוי בגופן, אנא ראה את המסמכים המפורטים הבאים בתקלת ו דוק, סיפרים ועוד:

- בלוג הבלוג הרשמי של Go: https://blog.golang.org
- טכנולוגיות הניפוי בGo: https://dave.cheney.net
- האתר הרשמי של Go: https://golang.org/