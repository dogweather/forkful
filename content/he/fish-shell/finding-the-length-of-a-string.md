---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
גילוי אורך של מחרוזת בפיש של (Fish Shell) הוא פעולה של החזרת מסם האותיות במחרוזת. מתכנתים מבצעים זאת לשימושים אינספור, דוגמת שליטה במחרוזות כניסה או כדי לממש אלגוריתמים מסוימים.

## איך לעשות?:
בשפת פיש של (Fish Shell), נשתמש בפקודה `string length`. נהוג להכניס את מחרוזת התווים לאחר `string length`.

```Fish Shell
string length "עברית"
```

פלט:
```Fish Shell
6
```

## צלילה עמוקה:
הפקודה `string length` היא חלק מספריה המובנית של פיש של (Fish Shell) שהושקה בגרסה 2.3.0 בשנת 2016ײ. מטרתה להקל על משימות עיבוד טקסט. שימוש באופרטור `string length` הוא דרך אחת למצוא את אורך המחרוזת, אך יתכן שקיימות אופציות אחרות, בהתאם לשפה שבה אתה עובד. על פיה עובדת הפונקציה, היא מניחה שהמחרוזת הוזנה נכונה, ומונה את התווים שבה.

## ראה גם:
למידע נוסף, אתה מוזמן לעיין בדוקומנטציה הרשמית של פיש של (Fish Shell) המסבירה על פקודת `string length`:
https://fishshell.com/docs/current/cmds/string-length.html