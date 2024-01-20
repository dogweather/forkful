---
title:                "שימוש בביטויים רגילים"
html_title:           "Fish Shell: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
באש, ביטויים רגולריים (RegEx) הם כלי חזק שנמנע לזהות מחרוזות בקוד. מתכנתים משתמשים בזה כדי להאיץ פעולות עיבוד טקסט ותנאים מורכבים.

## איך עושים זאת?
הנה כמה דוגמאות קוד באמצעות סקיפטים של Bash עם RegEx.

```Bash
#!/bin/bash
string="בדיקה123"
if [[ $string =~ [0-9] ]]
then
     echo "המחרוזת מכילה מספר."
else
     echo "המחרוזת לא מכילה מספר."
fi
```
הפלט:
`המחרוזת מכילה מספר.`

## עומק יותר
RegEx התפתח ראשית לשפת תכנות אסכמה, אבל מאז שולב ברוב שפות התכנות. ישנם גם כלים כמו grep בלינוקס שמאפשרים שימוש בביטויים מעשיים, אבל הם מוגבלים יותר מאשר בשפות תכנות מלאות. באש עשוי להיות שונה קלות באופן בו משתמש בו, אך הבנת היסודות של RegEx יכולה להיות מועברת לשפות תכנות אחרות.

## ראה גם
- [אתר מאמרים המסביר את RegEx](https://www.regular-expressions.info/) (באנגלית)
- [מספר סרטוני הדרכה](https://www.youtube.com/results?search_query=regex+tutorial) (באנגלית)
- [מסמך מתכנתים של GNU](https://www.gnu.org/software/grep/manual/grep.html) המסביר את הבאש בהקשר של RegEx (באנגלית)