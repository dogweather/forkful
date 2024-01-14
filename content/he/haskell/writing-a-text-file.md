---
title:    "Haskell: כתיבת קובץ טקסט"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה
כתיבת קובץ טקסט היא חלק חשוב מאוד מתהליך הפיתוח ב־הסקל. כתיבת טקסט נוחה ונכונה יכולה לשפר את התקשורת ולסייע בהתייחסות לבעיות ותכונות בקוד.

## כיצד לבצע
כדי לכתוב קובץ טקסט בהסקל, ניתן להשתמש בפונקציות פשוטות כמו `writeFile` ו־`appendFile`, ולציין את שם הקובץ ותוכנו. לדוגמה:

```Haskell
writeFile "text.txt" "זוהי שורת טקסט ראשונה כתובה בקובץ."
appendFile "text.txt" "וזוהי שורה נוספת שתתווסף לסוף הקובץ."
```

כאשר הקוד ירוץ, הקובץ "text.txt" יוצר עם שתי שורות טקסט בתוכו:

```
זוהי שורת טקסט ראשונה כתובה בקובץ.
וזוהי שורה נוספת שתתווסף לסוף הקובץ.
```

## מעמקים
ניתן להשתמש ב־`writeFile` ו־`appendFile` להוספת נתונים לקובץ קיים, או ליצירת קבצים חדשים. כמו כן, ניתן להשתמש בתיחום פונקציות נוספות כמו `putStrLn` ו־`putStr` להפקת תוכן למסך בזמן הריצה של הקוד.

## ראו גם
- [מדריך להסקל עבור מתחילים](https://edx.org/course/introduction-to-functional-programming-3)
- [מסמכי רשת לספרייה של הסקל](https://hackage.haskell.org/)
- [כתיבת טקסט ב־Haskell](https://wiki.haskell.org/Writing_text_files)