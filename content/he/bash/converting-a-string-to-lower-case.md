---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא פשוט להפוך את כל האותיות הגדולות לאותיות קטנות. תוכנתים מממשים את זה כדי להימנע מבעיות רגישות לרישיות בתוך הקוד שלהם.

## איך לעשות:
בהנחה שיש לנו מחרוזת שלך שנקראת `myStr`, כאן נראה איך להמיר את זה לאותיות קטנות:

```Bash
myStr="Hello World"
echo "${myStr,,}"
```

תוצאה תהיה:

```Bash
hello world
```

## הצצה לעומק:
מתחילים הרבה פעמים מניחים שהמרת מחרוזת לאותיות קטנות היא פשוט תהליך של איפוס של bit אחד. אך בעצם, המרה של אות לקטנה היא פעולה מורכבת אם מדובר בעולמות שפות מרובות.

ישנם שיטות אחרות להמיר מחרוזת לאותיות קטנות, כמו שימוש ב-`tr`:

```Bash
myStr="Hello World"
echo $myStr | tr '[:upper:]' '[:lower:]'
```

או אפילו מהלך מסובך יותר במקרה שאין זמינות למשתנים של Bash 4.0:

```Bash
myStr="Hello World"
echo $myStr | awk '{print tolower($0)}'
```

## ראה גם:
כדאי לבדוק את האתרים הבאים, שמכילים מידע נוסף ושימושי:

1. [Bash String Manipulation Guide](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
2. [Unix tr command](https://www.geeksforgeeks.org/tr-command-unixlinux-examples/)
3. [Awk Command in Unix](http://www.grymoire.com/Unix/Awk.html)