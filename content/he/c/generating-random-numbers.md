---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:05.091760-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
גילוי מספרים אקראיים ב-C זה ליצור ערכים שלא ניתן לחזות מראש. מתכנתים משתמשים בזה לצורכי בדיקות, משחקים, אבטחה, ועוד.

## How to (איך לעשות)
בראש ובראשונה, תכליתלי להוסיף הכותרת `#include <stdlib.h>` כדי להשתמש בפונקציה `rand()`, ואת `#include <time.h>` כדי להשתמש ב `time()` בתור זרע עבור הגנרטור האקראי.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(NULL)); // זריעה אקראית בהתבסס על הזמן
    for (int i = 0; i < 5; i++) {
        printf("%d\n", rand()); // מדפיס חמישה מספרים אקראיים
    }
    return 0;
}
```

הרצת הקוד תיצור פלט של חמישה מספרים אקראיים שונים בכל פעם.

## Deep Dive (צלילה עמוקה)
הפונקציה `rand()` מגיעה איתנו מימי ה-C הקדומים, והיא נותנת גישה ישירה למספרים אקראיים. חשוב לזכור שהיא לא נותנת אקראיות "אמיתית", אלא פסוודו-אקראיות. עבור גילוי אקראיות אמיתית יש להשתמש במקורות ייעודיים.

השתמשו בפונקציה `srand()` כדי לזרוע את הגנרטור כל פעם בתחילה; ללא זריעה, הסדרה של המספרים תשאר זהה בכל הרצה. בדרך כלל אנחנו אומרים שופשר להשתמש ב`time(NULL)` בתור זרע אבל זה לא ייתן אבטחה גבוהה.

לחלופין, במערכת הפעלה ישנים API ייעודיים לאקראיות גבוהה יותר, כמו `/dev/random` ו`/dev/urandom` בלינוקס או `CryptGenRandom` בווינדוס.

## See Also (ראה גם)
- [cplusplus.com: rand](https://www.cplusplus.com/reference/cstdlib/rand/)
- [cplusplus.com: srand](https://www.cplusplus.com/reference/cstdlib/srand/)
- [GNU C Library: ISO Random](https://www.gnu.org/software/libc/manual/html_node/ISO-Random.html)
- [Linux man page: random(4)](https://man7.org/linux/man-pages/man4/random.4.html)