---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות היא תהליך שמטרתו לבדוק את התכניות שלנו ולוודא שהן עובדות כפי שצריכות. המדיניות השיחקת מנתקת את הסרגל ממקום שווה ומכולם. 

## איך לעשות?

כתיבת בדיקות דורשת שמדיניות טוענת. זה מאפשר לנו לבדוק ולוודא שהקוד שלנו עובד כפי שצריך. להלן כמה דוגמאות:

```Bash
# בדיקת הצגת המשתנה הנרשם
name="יוסף"
echo $name
```

תוצאה: `יוסף`

```Bash
# בדיקת התו הראשון במחרוזת
string="שלום"
echo ${string:0:1}
```

תוצאה: `ש`

## העמק

הרקע ההיסטורי של כתיבת בדיקות התחיל בשנות ה-70 כשחברת AT&T כתבה מיקוד ברשתי TCsh. על אף זאת, כיום קיימות כמה אלטרנטיבות לכתיבת בדיקות, כגון JUnit לשפת ג'אווה ו-NUnit לשפת סי סי+. לפעמים כתיבת בדיקות מכובדת הנתונים מקצוע שהוערכו ושווים כלכלית לפני כבר קשורים לעולם.

## ראה גם

למידע נוסף ודוגמאות נוספות ניתן לעיין במדריכים ובמאמרים הבאים:

- שיעורי Bash באתר Codeacademy: https://www.codecademy.com/learn/learn-bash
- מאמר "כיצד לבצע בידיקות התקנה כספית עבור הפרויקט שלכם" באתר Better Programming: https://betterprogramming.pub/how-to-test-financial-installment-for-your-project-ce87dd32e8d8