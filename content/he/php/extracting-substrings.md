---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:46:16.212230-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה "חילוץ תת-מחרוזות" ולמה זה חשוב? חילוץ תת-מחרוזות זה לקחת חתיכה מתוך מחרוזת. תכנותים עושים את זה כדי לעבד או לבדוק נתונים ספציפיים בתוך מחרוזת גדולה יותר.

## How to:
```PHP
<?php
// דוגמה לחילוץ תת-מחרוזת ב-PHP

$str = "שלום עולם, PHP כאן לשרת אתכם!";
echo substr($str, 6, 5); // Outputs: עולם

// חילוץ מתחילת המחרוזת עד תו מסוים
echo substr($str, 0, 5); // Outputs: שלום 

// חילוץ תת-מחרוזת מסוף המחרוזת
echo substr($str, -3); // Outputs: אתכם
?>
```
רק תזכרו, בעברית יש עניין של תווים מיוחדים בצורת RTL (Right-To-Left).

## Deep Dive
חילוץ תת-מחרוזת היא פעולה שנעשית מאז ומתמיד בתכנות. ב-PHP, הפונקציה `substr` היחידה איתה אנחנו מתעסקים פה, קיימת מאז PHP 3. אלטרנטיבות כוללות את הפונקציות `mb_substr` לתמיכה במבנה רב-ביתי (למשל, UTF-8) שחשוב במיוחד כשמתעסקים עם תווים בעברית או שפות אחרות שאינן לטיניות. יש גם אפשרות להשתמש בביטויים רגולריים עם `preg_match` אם צריך לחילוץ תת-מחרוזות מורכב יותר.

## See Also
- התיעוד הרשמי של PHP לפונקציית `substr`: https://www.php.net/manual/en/function.substr.php
- התיעוד הרשמי של PHP לפונקציית `mb_substr`: https://www.php.net/manual/en/function.mb-substr.php
- חילוץ תת-מחרוזות באמצעות ביטויים רגולריים עם `preg_match`: https://www.php.net/manual/en/function.preg-match.php
