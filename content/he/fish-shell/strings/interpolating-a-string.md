---
title:                "שרבוב מחרוזת"
aliases:
- /he/fish-shell/interpolating-a-string/
date:                  2024-01-20T17:50:47.806901-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת מאפשרת לנו להטמיע ערכים של משתנים בקלות תוך כדי כתיבה של המחרוזת. זה שימושי כשאנחנו רוצים להפוך את הקוד ליותר גמיש ולהתאים את הפלט להקשרים שונים.

## איך לעשות:
```Fish Shell
# דוגמה לאינטרפולציה של מחרוזת
set name "דורון"
echo "שלום, $name"
```

תוצאה:
```
שלום, דורון
```

בואו נשלב משהו מורכב יותר:
```Fish Shell
set item "שוקולד"
set price 5
echo "המחיר של $item הוא $price שקלים"
```

תוצאה:
```
המחיר של שוקולד הוא 5 שקלים
```

## עומק ים
אינטרפולציה של מחרוזות היא כלי שהתפתח עם השנים בשפות תיכנות שונות. ב-Fish Shell, הדרך שבה אתה משלב משתנים במחרוזות היא ישירה ואינטואיטיבית - פשוט מציב את שם המשתנה בתוך גרשיים.

יש שפות אחרות, כמו PHP וPerl, שבהן זה נעשה אחרת, אבל ב-Fish זה פשוט וללא כאב ראש. מבחינת ביצועים, Fish מטפל באינטרפולציה באופן אפקטיבי. לא צריך לדאוג שהשימוש במשתנים במחרוזת יאט את הסקריפט שלך.

## ראו גם
- הדוקומנטציה הרשמית של Fish Shell על מחרוזות וגרשים: [https://fishshell.com/docs/current/index.html#syntax-string](https://fishshell.com/docs/current/index.html#syntax-string)
- מדריך מבוא ל-Fish Shell למתחילים: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
