---
title:                "Python: כיתוב מלא עבור מחשבים"
simple_title:         "כיתוב מלא עבור מחשבים"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# מדוע:

בפייתון, השימוש בפונקציה `capitalize()` על מחרוזת מאפשר לנו להחזיר את המחרוזת עם האות הראשונה מיוחדת. כך תוכלו להשתמש בפונקציה זו כדי לייצר עיצוב מיוחד של מחרוזת במהלך ביצועי התוכנית שלכם.

## כיצד לבצע זאת:

```Python
# הדפסת מחרוזת מקורית
print("hello world")
# השימוש בפונקציה capitalize על מחרוזת והדפסת התוצאה
print("hello world".capitalize())
```

### פלט:

```Python
Hello world
```

## ביצוע עמוק:

במקרים מסוימים, כשיש בעיה עם אות הראשונה במחרוזת, כפי שהיא נמצאת במחברת זו כעת, תוכלו להשתמש בפונקציה המתקדמת `title()` כדי לטפל במחרוזת כולה ולהחזיר את האות הראשונה של כל מילה כמו שצריך.

### פלט:

```Python
Hello World
```

# ראו גם:

בשביל לתפוס יותר את הנושא של ההופכיות במחרוזות בפייתון, תוכלו לקרוא את המדריך על `swapcase()` באתר העזרה הרשמי של פייתון:
https://docs.python.org/3/library/stdtypes.html#str.swapcase

ובתרגום לעברית:
https://docs.python.org/3/library/stdtypes.html#str.swapcase