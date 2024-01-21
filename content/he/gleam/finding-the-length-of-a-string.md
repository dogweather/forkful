---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:58.696620-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת זה פשוט ניתוח של מספר התווים שבה. תכניתנים עושים זאת כדי לבדוק נתונים, לוודא תקינות, ולעבד מידע בצורה יעילה.

## איך לעשות:
```gleam
import gleam/string

pub fn main() {
  let greeting = "שלום, עולם!"
  let length = string.len(greeting)
  length
}
// הפלט יהיה: 12
```

כאן אנחנו רואים דוגמא של קוד ב-Gleam שמחשב את אורך המחרוזת.

## עיון מעמיק
בעבר, למציאת אורך של מחרוזת היו תלויים בפונקציות רמה נמוכה שסופרות תווים עד שמוצאות סימן סיום. היום, ב-Gleam ובשפות רבות אחרות, זה מובנה וקל לשימוש. יתכנו חלופות כמו פונקציות ידניות אופטימיזציה אבל לא בהכרח צריך להשתמש בהן. חשוב לזכור שבעת מציאת אורך מחרוזת עלולים להיות מוסבכים בשפות המשתמשות בתווים יוניקוד כי מונים תווים ולא תווים גרפיים.

## לראות גם
- מסמכי Gleam: https://gleam.run/docs/
- מקורות על יוניקוד: https://unicode.org/
- טיפים לעבודה עם מחרוזות בצורה יעילה: https://use-the-index-luke.com/sql/where-clause/searching-for-ranges/like-performance-tuning