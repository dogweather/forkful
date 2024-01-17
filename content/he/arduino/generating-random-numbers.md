---
title:                "יצירת מספרים אקראיים"
html_title:           "Arduino: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

הפיתוחנים נדרשים ליצור מספרים אקראיים בעבודתם על מכשירי ארדואינו. מתי ולמה זה חשוב? בפשטות - מספרים אקראיים מאפשרים לנו ליצור פעולות מקריות בקוד וליצור יישומים יותר דינמיים ומעניינים.

## איך לעשות זאת:

נשתמש בדוגמה פשוטה כדי להדגים איך ליצור מספרים אקראיים בארדואינו. שימו לב שהשורות המסומנות בסימן גרשיים אפשרות להיכנס לקוד ולנסות אותו בעצמכם.

```arduino
// אתחול הגנרטור האקראי
randomSeed(analogRead(0));

// יצירת מספר אקראי בין 1 ל-10
int randomNum = random(1, 11);

// הדפסת המספר האקראי במסך
Serial.println(randomNum);
```

הפלט שנקבל הוא מספר אקראי בין 1 ל-10 כל פעם שנכבה את המכשיר.

## פירור עמוק:

הגרסה הקודמת של ארדואינו כנראה מכילה גנרטור אקראי פשוט יותר מזו שכעת בשימוש. בין האפשרויות האלטרנטיביות ליצירת מספרים אקראיים בארדואינו ניתן למצוא את הספרייה ```randomSeed()``` שמאפשרת לסנכרן את הגנרטור עם חומרה חיצונית או עם שעון פנימי.

## ראו גם:

ניתן למצוא מידע נוסף ודוגמאות נוספות על יצירת מספרים אקראיים בארדואינו באתר המקורי של ארדואינו: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/

להמשך קריאה על גנרטורים אקראיים ושימושים בהם בתכנות ניתן לבדוק את המקורות המומלצים הבאים:

1. https://en.wikipedia.org/wiki/Random_number_generation
2. https://blog.cloudflare.com/randomness-101-lavarand-in-production/
3. https://www.computerhope.com/jargon/r/retosoal.htm