---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

מציאת אורך של מחרוזת הוא פעולה של ספירת התווים שבה. תוכנתים עושים את זה כדי לקבוע את מידת הנתונים במחרוזת, או לבצע בה פעולות ספרתיות מסוימות.

## איך ל:

הנה דוגמא לקוד ב-Gleam שמוצא את אורך המחרוזת:

```gleam
fn main() {
    let str = "שלום";
    let length = length(str);
    println!("אורך המחרוזת הוא {}", length);
}
```

פלט של הדוגמא לעיל הוא:

```
אורך המחרוזת הוא 4
```

## הצצה עמוקה 

1. בהיסטוריה: פעולת מציאת אורך של מחרוזת היא אחת הפעולות הראשונות שממומשות בתכנות. היא גם אחת הבסיסיות המשותפות לרוב השפות. 

2. חלופות: בכמה שפות, זה מתבצע ישירות על-ידי שימוש בפונקציה המובנית 'length'. באחרות, אנו מניחים שהמחרוזת מסתיימת בתו null ונמדדים אורכה.

3. קצת פרטים על היישום: ב-Gleam, מציאת אורך מחרוזת היא פעולה O(1). זאת אומרת שעלויות המחשבת של הפעולה הן קבועות, לא משנה כמה גדולה המחרוזת.

## ראו גם 


- [ספר מתכנתים של Gleam](https://gleam.run/book/) - ספר חינם שמדריך דרך חלק מהמאפיינים של השפה.

לעומק יותר בנושא, ראו את המדריך המקוון המפורט של [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length).