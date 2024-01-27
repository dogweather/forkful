---
title:                "הסרת מרכאות ממחרוזת"
date:                  2024-01-26T03:40:10.235950-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

מה ולמה?
הסרת מרכאות ממחרוזת משמעותה התקלפות של שכבות אלו הנוספות - סימני הציטוט - מהנתונים הטקסטואליים שלך. מתכנתים עושים זאת כדי לחטא את הקלט, להכין מחרוזות לעיבוד, או פשוט כדי לשמור על דברים מסודרים ועקביים ביישומים שלהם. מדובר בנתונים נקיים ושמישים בסופו של דבר.

איך לעשות זאת:
הסרת מרכאות בGleam היא פשוטה. אנו יכולים להשתמש בהתאמת תבניות או בפונקציות מחרוזת מובנות. הנה דוגמה מהירה להמחשה:

```gleam
pub fn remove_quotes(text: String) -> String {
  let without_quotes = string.trim(text, "\"")
  without_quotes
}

pub fn main() {
  let text_with_quotes = "\"Hello, World!\""
  let cleaned_text = remove_quotes(text_with_quotes)
  io.println(cleaned_text)
}
```

פלט לדוגמה:
```
Hello, World!
```

צלילה עמוקה
באופן היסטורי, עיסוק במרכאות במחרוזות היה משימה נפוצה בעיבוד טקסט ובשפות תסריטים. בשל העובדה שמחרוזות לעיתים קרובות הן קלט מהמשתמש או נקראות מקבצים, הן עשויות להגיע עם סימני ציטוט שצריך להסיר מסיבות שונות, כמו הכנסה למסד נתונים או עיצוב.

בGleam, אנו משתמשים בפונקציה `string.trim` כדי להסיר את הציטוטים. ישנן אלטרנטיבות! יכולנו לרוץ דרך המחרוזת או להחיל ביטויים רגולריים, אך `string.trim` היא הכלי הנוח ביותר לעבודה בזכות מינימליותה וביצועיה.

אם אנו צוללים לפרטי המימוש, `string.trim` פועלת על ידי הסרת תווים מתחילת וסוף המחרוזת שתואמים את התבנית המסופקת. אז אם יש לך ציטוטים בשני קצוות המחרוזת, הם יוסרו בבת אחת. שים לב שהיא מסירה את הציטוטים רק אם הם נמצאים בקצוות; ציטוטים שנמצאים באמצע הטקסט שלך ישארו במקומם.

ראה גם
לנפשות הסקרניות שבינינו שרוצות לחקור עוד:
- [תיעוד מודול המחרוזת של Gleam](https://gleam.run/stdlib/string/)
- [עוד על התאמת תבניות בGleam](https://gleam.run/book/tour/pattern-matching)
- דיונים על עיבוד טקסט בתכנות ב[Stack Overflow](https://stackoverflow.com/questions/tagged/text-processing)