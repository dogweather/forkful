---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

#בהירור והחלפת טקסט ב-Gleam: מה ולמה?

בהירור והחלפת טקסט הן פעולות בסיסיות בתכנות. בעזרתן אנו מצליחים למצוא או להחליף מחרוזת מסוימת בתוך טקסט. לתכנתנים אנו עושים את זה כדי לשנות או לשדרג קוד, למצוא שגיאות או לאפשר שינויים דינמיים.

#איך לעשות

Gleam מכניסה מידע בקריאות וידור, כולל לתוך טקסט. נראה איך זה נראה בקוד:

```gleam
import gleam/string

fn main() {
  let str = "ברוך הבא לעולם של Gleam!"
  let new_str = string.replace("Gleam", "תכנות", str)
  new_str
}
```
הפלט יהיה: `"ברוך הבא לעולם של תכנות!"`

#בהירור והחלפה: צלילה עמוקה

מאז התקופה של ה-Unix, כגון `sed` ו-`awk` מבצעים פעולות של בירור והחלפת טקסט. Gleam מציעה שיטה פונקציונלית לגישה זו. מעבר למתודה string.replace שהראינו, Gleam מציעה מתודות נוספות כמו `replace_slice`, המאפשר להחליף סוף או התחלה של מחרוזת.

כמו כל שפת תכנות, גם ב-Gleam לביצוע החלפה יש השלכות ביצועים. בתחילה, יש לבחון את המחרוזת. לאחר מכן, עבור כל מחרוזת שאנחנו מחפשים, אנו נאלצים להכין מחרוזת חדשה שמכילה את השינוי.

#ראה גם

* [תיעוד ה-Immutable.js הרשמי](https://docs.rs/gleam/0.1.1/gleam/string/fn.replace.html) - מסביר את פעולות המחרוזת המרכזיות.
* [הובלת קוד והחלפת וידור](https://www.gleam.run/book/tour/pattern-matching.html) - מדריך מעשי למימוש החלפת טקסט מותאם אישית ב-Gleam.