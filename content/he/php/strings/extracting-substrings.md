---
aliases:
- /he/php/extracting-substrings/
date: 2024-01-20 17:46:16.212230-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \"\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA\
  -\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\" \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4\
  \ \u05D7\u05E9\u05D5\u05D1? \u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D6\u05D4 \u05DC\u05E7\u05D7\u05EA \u05D7\u05EA\
  \u05D9\u05DB\u05D4 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  . \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05D0\u05D5 \u05DC\
  \u05D1\u05D3\u05D5\u05E7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E1\u05E4\u05E6\
  \u05D9\u05E4\u05D9\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\u2026"
lastmod: 2024-02-18 23:08:52.920359
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \"\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA\
  -\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\" \u05D5\u05DC\u05DE\u05D4 \u05D6\u05D4\
  \ \u05D7\u05E9\u05D5\u05D1? \u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D6\u05D4 \u05DC\u05E7\u05D7\u05EA \u05D7\u05EA\
  \u05D9\u05DB\u05D4 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  . \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05D0\u05D5 \u05DC\
  \u05D1\u05D3\u05D5\u05E7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E1\u05E4\u05E6\
  \u05D9\u05E4\u05D9\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\u2026"
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
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
