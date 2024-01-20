---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים מתאימים לתבנית היא כלי נפוץ בתיכנות. זה מאפשר לנו להסיר מידע לא קריטי, לנקות נתונים לפני ניתוח, או לפשט משימות שונות.

## איך לעשות:
אפשר למחוק תווים מתאימים לתבנית באמצעות שורת הפקודה ב-Fish Shell. ראו את הדוגמה למטה. 

```
Fish Shell

# בניית מחרוזת
set str "Hello, World!"

# מחיקת כל התווים מהמחרוזת שלא הם אותיות
echo $str | string replace -r '[^a-zA-Z]' ''

ריצה של הקוד הנ"ל תחזיר: 
HelloWorld
```

## צלילה עמוקה
מחיקת תווים מתאימים לתבנית היא רעיון מתכנתים שמשמש למעלה משני עשורים. ניסוי וטעות הם חלק מהתרבות של התיכנות, והכלי הזה הוא פלט של זה. יתר על כן, ישנן גרסאות אלטרנטיביות ל-Fish Shell כמו Bash או Zsh שמתמקדות באופציות דומות. ביחס לפרטי היישום: Fish Shell מתמחה במחיקת התווים המתאימים באמצעות ביטויים רגולריים.

## למידע נוסף
למידע מעמיק יותר על מחיקת תווים לפי תבנית, אפשר לראות את המקורות הבאים:

1. [דוקומנטציה של Fish Shell](https://fishshell.com/docs/current/commands.html)
2. [הדרכה על ביטויים רגולריים](https://www.regular-expressions.info/)