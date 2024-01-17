---
title:                "מחיקת תווים המתאימים לתבנית"
html_title:           "Python: מחיקת תווים המתאימים לתבנית"
simple_title:         "מחיקת תווים המתאימים לתבנית"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

מה ולמה?
מחיקת תווים התואמים לתבנית היא תהליך בו תווים מסוימים נמחקים מהקוד לפי תבנית מסוימת. תהליך זה נעשה בכדי לנקות את הקוד מתווים לא רלוונטיים שעלולים לגרום לטעויות.

איך לעשות:
```Python
# מחיקת תווים תואמים לתבנית באמצעות הפונקציה re.sub
import re
my_string = "Hello, world!"
# נמחק את התו comma באמצעות התבנית המתאימה
new_string = re.sub(r",", "", my_string)
print(new_string)
# פלט: Hello world!
```

```Python
# מחיקת תווים שמופיעים מיותרים בסוף מחרוזת מסוימת
my_string = "Hello, world!......."
# נמחק את כל התווים של הנקודות בסוף המחרוזת
new_string = my_string.rstrip(".")
print(new_string)
# פלט: Hello, world!
```

עומק נרחב:
תוספת המנוע הפיתוח של פייתון - re תרומה במילים רבות לאפשרות מחיקת תווים בפייתון, עם אפשרויות רבות מגוונות וגמישות. במקום למחוק תווים בפונקציה, ישנן אפשרויות אחרות זמינות כגון חילופים שקולים, הכנסת תווים או הוספת תווים במקום מחיקה. ישנם גם תוספים נוספים כמו יכולת להגדיר תבניות מתוכנתות באופן אישי.

ראו גם:
https://docs.python.org/3/library/re.html - התיעוד הרשמי של פייתון עבור תוסף המנוע re.
https://www.programiz.com/python-programming/regex - מדריך מפורט על כתיבת תבניות בפייתון.