---
title:                "מחיקת תווים התואמים תבנית"
html_title:           "Elm: מחיקת תווים התואמים תבנית"
simple_title:         "מחיקת תווים התואמים תבנית"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

למה אנשים ימחקו תווים התואמים דפוס בתכנות Elm? ייתכן שיש להם מחרוזת שמכילה תווים לא רצויים והם רוצים להסיר אותם, או שהם רוצים לבצע תהליך אוטומטי כדי לנקות את המחרוזת מתווים מיותרים.

## איך לעשות זאת

למחוק תווים בתוכנית Elm ניתן להשמיט את התווים הרצויים עם עזרת פונקציות כמו `String.dropLeft` ו`String.dropRight`. למשל, אם נרצה להסיר את התווים '$' מהמחרוזת "abc$123", נוכל לכתוב:

```elm
String.dropLeft 1 "abc$123"
```

תוצאה: "ab123"

כמו כן, ניתן להשתמש בפונקציה `String.replace` כדי להחליף את התווים הרצויים בתווים אחרים. לדוגמה, אם נרצה להחליף את התווים '&' בתווים '_' במחרוזת "abcdefg&hijklmnop", נוכל לכתוב:

```elm
String.replace "&" "_" "abcdefg&hijklmnop"
```

תוצאה: "abcdefghijklmnop"

ניתן גם להשתמש בתכונה המובנית של שפת Elm להפעלת פונקציות על מחרוזות באמצעות רצף מקוצר של פעולות. לדוגמה, נרצה להחליף את התווים 'x' בתווים 'o' במחרוזת "xxxxxx", נוכל לכתוב:

```elm
"xxxxxx"
|> String.replace "x" "o"
```

תוצאה: "oooooo"


## חקר עמוק

ניתן להשתמש גם בתכונה המתקדמת של שפת Elm לעבוד על מחרוזות באמצעות הפונקציות `String.dropWhile` ו`String.dropUntil`. הן מאפשרות למחוק תווים במספר מצבים שונים, כגון בהתאם לתנאי מסוים או ביחס למחרוזת אחרת. לדוגמה, אם נרצה למחוק את התווים המתחילים באות 'a' מהמחרוזת "abcdef123", נוכל לכתוב:

```elm
String.dropUntil (\c -> c == 'a') "abcdef123"
```

תוצאה: "bcdef123"

כמו כן, `String.dropWhile