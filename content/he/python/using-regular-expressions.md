---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה ביטויים רגולריים, ולמה מתכנתים משתמשים בהם? ביטויים רגולריים (Regular Expressions או Regex) זה כלי לחיפוש והחלפת טקסט על פי דפוסים. מתכנתים משתמשים בהם כי הם מאפשרים עיבוד טקסט מתקדם ומהיר בצורה אוטומטית.

## How to:
יצירת ביטוי רגולרי:
```Python
import re

pattern = re.compile(r'\b[A-Za-z]+\b')
text = "Regex בפייתון מאפשר עבודה עם טקסט!"
matches = pattern.findall(text)
print(matches)
```
פלט:
```
['Regex', 'בפייתון', 'מאפשר', 'עבודה', 'עם', 'טקסט']
```

החלפת תוכן באמצעות ביטוי רגולרי:
```Python
replaced_text = re.sub(r'\bטקסט\b', 'מילים', text)
print(replaced_text)
```
פלט:
```
Regex בפייתון מאפשר עבודה עם מילים!
```

## Deep Dive
ביטויים רגולריים נוצרו בשנות ה-50 ומאז השתלבו ברוב שפות התכנות. אלטרנטיבות כוללות עבודה עם טקסט באמצעות מתודות כמו `split()` ו`find()`, אבל הן פחות גמישות מביטויים רגולריים. פייתון משתמשת בספרייה `re` לעבודה עם regex, שמבוססת על ספריית PCRE (Perl Compatible Regular Expressions) שמציעה דפוסים חזקים ומתקדמים.

## See Also
- [תיעוד ביטויים רגולריים של Python](https://docs.python.org/3/library/re.html)
- [Regex101](https://regex101.com/) - לבדוק וללמוד ביטויים רגולריים
