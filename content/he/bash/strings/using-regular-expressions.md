---
title:                "שימוש בביטויים רגולריים"
aliases:
- /he/bash/using-regular-expressions/
date:                  2024-02-03T19:16:38.620720-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) ב-Bash מאפשרים לך לחפש, לעבד, ולהתמודד עם מחרוזות וקבצים על סמך תבניות מסוימות. תכנתים משתמשים ב-regex למשימות כמו אימות קלט, פיענוח קבצי לוג, וחילוץ נתונים מפני שהוא מציע דרך גמישה ועוצמתית לציין תבניות לצרכי עיבוד טקסט מורכבים.

## איך ל:

### התאמת תבניות בסיסית
כדי לבדוק אם מחרוזת מתאימה לתבנית, תוכל להשתמש ב-`grep`, כלי שורת פקודה לחיפוש בערכות נתוני טקסט פשוטות עבור שורות שמתאימות לביטוי רגולרי:

```bash
echo "Hello, World!" | grep -o "World"
# פלט: World
```

### חילוץ נתונים ספציפיים
כדי לחלץ חלקים מנתונים שמתאימים לתבניות regex שלך, תוכל להשתמש ב-`-o` עם `grep`:

```bash
echo "Error: File not found" | grep -oE "[A-Za-z]+:"
# פלט: Error:
```

### שימוש ב-Regex עם `sed`
`sed` (עורך זרם) הוא כלי עוצמתי לניתוח והמרת טקסט. הנה איך להשתמש ב-`sed` עם regex כדי להחליף טקסט:

```bash
echo "Bash is great" | sed -e 's/great/awesome/'
# פלט: Bash is awesome
```

### התאמת תבניות בהצהרות תנאיות
-Bash תומך גם ב-regex בהצהרות תנאיות ישירות:

```bash
[[ "https://example.com" =~ ^https?:// ]] && echo "URL is valid" || echo "URL is invalid"
# פלט: URL is valid
```

### התאמת תבניות ומניפולציה מתקדמת עם `awk`
`awk` הוא כלי נוסף לעיבוד טקסט שתומך בחילוץ ומניפולציה של נתונים מורכבים יותר. זה יכול להיות שימושי במיוחד כאשר עובדים עם נתוני טקסט מובנים, כמו CSV:

```bash
echo -e "ID,Name,Age\n1,John,22\n2,Jane,24" | awk -F, '$3 > 22 {print $2 " is older than 22."}'
# פלט: Jane is older than 22.
```

בעוד שפונקציונליות ה-regex המובנית של Bash כוללת מגוון רחב של שימושים, למשימות regex מתקדמות מאוד, תוכל לשקול להשתמש בשילוב של סקריפטים ב-Bash עם סקריפטים ב-`perl` או `python`, מכיוון ששפות אלו מציעות ספריות regex עוצמתיות (למשל, `re` ב-Python). דוגמה פשוטה עם Python:

```bash
echo "Capture this 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# פלט: 123
```

הטמעת שפות תכנות אלו כשצריך יכולה לעזור לך לנצל את הכוח המלא של regex בסקריפטים שלך ב-Bash.
