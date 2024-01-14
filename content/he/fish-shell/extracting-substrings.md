---
title:                "Fish Shell: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

# עבור מה:
הפעולה של חילוץ תת-מחרוזות מנתונים טקסטואליים יכולה להיות מועילה למתכנתים שמשתמשים בקוד של כמה שפות תכנות שונות. זה יכול לסייע להפוך את הקוד לפשוט ומיומן יותר על ידי חילוץ רק מה שצריך מהנתונים והתעלמות מכל ההשאר.

# איך לעשות זאת:
כדי לחלץ תת-מחרוזות מנתונים טקסטואליים בFish Shell, ניתן להשתמש בפונקציית `string sub` ולהזין אותה עם שני פרמטרים - הטקסט המקורי והקוד שמציין את התת-מחרוזת שצריך לחלץ.

```Fish Shell
set originalString "שלום לכולם"
set substring (string sub $originalString 4 7)
echo $substring
```
הפלט של הפקודה האחרונה יהיה `לום`.

אם נרצה לחלץ גם את התחלת המחרוזת, נוכל להשתמש בפונקציית `string length` כדי לקבל את אורך המחרוזת ולהשתמש בו בתור המגדירים התחלתיים וסופיים לתת-מחרוזת.

```Fish Shell
set originalString "שלום לכולם"
set length (string length $originalString)
set substring (string sub $originalString 4 (expr $length - 5))
echo $substring
```
תוצאת הפלט תהיה המחרוזת המקורית מלבד המילה "לכולם".

# הערות נוספות:
חילוץ תת-מחרוזות בFish Shell נעשה באמצעות פונקציות של Snooze, האם שמותיוו של הפונקציות מסבירים מה הפונקציה מבצעת. בנוסף, ניתן להשתמש בפונקצייה `string index` כדי למצוא את המיקום שמתחיל ממנו התת-מחרוזת הנדרשת.

# ראה גם:
- [מדריך לFish Shell](https://esev.com/blog/post/2015-01-pimp-my-terminal-fish/)
- [תיעוד רשמי של Fish Shell](https://fishshell.com/docs/current/)
- [מדריך לחילוץ תת-מחרוזות בשפת Perl](https://perldoc.perl.org/functions/