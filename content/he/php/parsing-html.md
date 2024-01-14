---
title:                "PHP: פענוח HTML"
simple_title:         "פענוח HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/parsing-html.md"
---

{{< edit_this_page >}}

**למה**: ניתן להשתמש בניתוח HTML כדי לגשת לתוכן של אתרים ולהציג אותו בצורה יעילה ומסודרת באפליקציות ובאתרים שלנו.

## איך לעשות את זה?

בכדי לפצח את קוד ה-HTML של דף אינטרנט ולהציג אותו בצורה ממודרנית ונקייה, ניתן להשתמש בפונקציה המובנית של PHP הנקראת "file_get_contents". באמצעות פקודה זו, ניתן לקרוא את קובץ ה-HTML ולשמור אותו במשתנה. לאחר מכן, ניתן להשתמש בפונקציה המובנית "preg_match_all" כדי לחפש ולקבוע את התוכן שיש לו תגית מסוימת ולהציג אותו בצורה מובנית.

```PHP
$html = file_get_contents( "https://www.example.com" );
preg_match_all( '|<h1>(.*?)</h1>|', $html, $output );

echo $output[1][0]; // יש להמיר המידע המקודם לצורה כזו כדי להציג את התוכן המבוקש במקום שנמצאים התגיות.
```

פקודה זו תחלץ את התוכן של תגית ה-H1 מתוך הקוד המקורי של האתר ותציג אותו בצורה נקייה ללא כל התרגילים והתוכן שאינו רלוונטי.

## נסתק עמוק יותר

ניתן להשתמש בפונקציה "preg_match_all" עבור תגיות אחרות גם כן, למשל "img" עבור תמונות, "a" עבור קישורים, ועוד. פקודה זו נחשבת לכלי חזק ויעיל להשתמש בו לצרכי ניתוח והצגת תוכן באתרים ובאפליקציות.

## ראה גם

- [דף תיעוד מלא עבור פונקציית "preg_match_all"](https://www.php.net/manual/en/function.preg-match-all.php)
- [פרק מתאר את גישת הדגימה של "preg_match_all" מתוך ספר הלימוד המקוון "PHP: The Right Way"](https://phptherightway.com/chapters/05-regex.html#file_get_contents-regexp)