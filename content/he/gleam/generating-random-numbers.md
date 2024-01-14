---
title:    "Gleam: יצירת מספרים אקראיים"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## למה

מחשבות עיוורון אינן מתקיימות בתופס המוח. בעולם התכנות, ייתכן שנתקל בצורך להשתמש במספרים אקראיים. זה עשוי להיות מכיוון שאתה מתכנת משחק, ואתה צריך ליצור אירועים אקראיים, או שאתה חוקר נתונים וצריך לדגום נתונים שאינם קבועים ומשתנים. בכל מקרה שלא תהיה, גריסות הופעות אקראיות הן כלי חשוב בתעשיית התכנות.

## איך לעשות

אם אתה משתמש בשפת תכנות Gleam, ליצור מספרים אקראיים פשוט וקל. ראשית, עליך להוריד את החבילה `gleam/random` מתוך גיטהאב ולהוסיף אותה לפרויקט שלך. לאחר מכן, תוכל להשתמש בפונקציה `random.float` על מנת ליצור מספרים אקראיים בין 0 ל1.

```Gleam
import random

fn generate_random_float() {
  let random_num = random.float(0, 1)
  io.println("The random float is: {:?}", random_num)
}
```

פלט משוער:

```
The random float is: 0.628476
```

כדי ליצור מספרים אקראיים בין ערכים נתונים, כגון כיסוי מספרים אקראיים, נעשה שימוש בפונקציה `random.int` ונציין את הערכים המינימלי והמקסימלי.

```Gleam
import random

fn generate_random_int() {
  let random_num = random.int(1, 10)
  io.println("The random integer is: {:?}", random_num)
}
```

פלט משוער:

```
The random integer is: 7
```

## הייתף

כדי לקבל צפייה מלאה בנושא הפקת מספרים אקראיים, כדאי ללמוד עוד על מחוללי מספרים אקראיים ועל השפה Gleam. בדקו את הלינקים הבאים על מנת להתחיל:

- [חבילת random של Gleam בגיטהאב](https://github.com/gleam-lang/gleam/blob/master/lib/random/src/random.gleam)
- [השפה Gleam בויקיפדיה](https://en.wikipedia.org/wiki/Gleam_(programming_language))
- [מסמכי הצ