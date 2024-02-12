---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:21.504480-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

מציאת אורך שרשרת היא התהליך שבו אנו סופרים את התווים בשרשרת טקסט (string). תכניתנים עושים זאת כדי לבצע ולידציה, לקבוע אורך שדות טקסט, או לנהל לוגיקה ספציפית למספר תווים.

## איך לעשות:

לדוגמה, אם יש לכם מחרוזת ואתם רוצים לדעת כמה תווים יש בה, תשתמשו בפקודה `string length`.

```Fish Shell
set my_string "שלום עולם"
echo $my_string | string length
```

פלט:

```
10
```

הפקודה משיבה 10, שזה אורך השרשרת, כולל הרווח.

## טבילה עמוקה

מציאת אורך שרשרת היא פעולה בסיסית אבל קריטית בכל שפת תכנות. ב-Fish Shell, `string length` פועלת על שרשרות ישירות או קלטים מפקודות אחרות דרך pipeline (|). בניגוד לשפות אחרות שבהן אורך השרשרת נמצא בתוך המשתנה עצמו, ב-Fish אתם צריכים לשלוח את השרשרת לתכנית העוסקת באורך השרשרות, וזהו - פשוט וקל. ללא צורך בלולאות מסורבלות או בדיקות ידניות.

## ראה גם

- מדריך רשמי של Fish Shell על מניפולציות שרשרת: https://fishshell.com/docs/current/cmds/string.html
- מאמרים על Fish Scripting: https://fishshell.com/docs/current/tutorial.html
- פורום תמיכה של Fish: https://fishshell.com/docs/current/index.html#discussion
