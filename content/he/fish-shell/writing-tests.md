---
title:                "כתיבת בדיקות"
html_title:           "Fish Shell: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## למה

כדי לוודא שהקוד שלנו עובד כמו שצריך ולהפסיק טעויות עתידיות.

## איך לכתוב ב Fish Shell
```Fish Shell
function test -d "This is a fish test"
  echo "This is a test."
end
```

```Fish Shell
test
```

```Fish Shell
Expected Output: "This is a test."
Actual Output: "This is a test."
```

## התעמקות

כתיבת בדיקות היא חלק חשוב מתהליך הפיתוח ויכולה לעזור לנו למצוא באופן מהיר את הבעיות בקוד שלנו. כתיבת טסטים מאפשרת לנו גם להדגים לאנשים אחרים איך להשתמש בקוד שלנו ומה הפלט המצופה שלו.

## ראה גם

* [כתיבת טסטים ב Fish Shell על ידי טיול באוניברסיטה](https://fishshell.com/docs/current/tutorial.html#writing-tests)
* [מדריך מלא ל Fish Shell](https://fishshell.com/docs/current/index.html)