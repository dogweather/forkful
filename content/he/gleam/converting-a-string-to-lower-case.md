---
title:                "Gleam: המרת מחרוזת לאותיות קטנות"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

### למה: 

המרת מחרוזת לאותיות קטנות היא תהליך נפוץ שמשמש לשיפור ביצועים וקריאות בקוד. כך ניתן לקבל קלט טקסטואלי ולהמיר אותו למחרוזת עם אותיות קטנות בלבד.

### איך לעשות:

דוגמאות קוד ופלט למשתמשים של Gleam נמצאים מתחת לחסימת הקוד הבאה:

``` Gleam
fn main() {
    let input = "HELLO WORLD";
    let output = input.to_lowercase();

    // פלט: "hello world"
    io.print(output);
} 
```

### לחקור עמוק:

המרת מחרוזת לאותיות קטנות היא תהליך פשוט וחשוב בתכנות. ניתן להיעזר בפונקציות כמו `to_lowercase()` ו- `to_casefold()` כדי לתמוך במגוון שפות וסביבות שונות. בנוסף, כדאי לוודא כי ההכנסה היא מחרוזת כדי למנוע שגיאות לא צפויות בעת ההרצה.

### ראה גם:

- מדריכי התחלה ל-Gleam: https://gleam.run/getting-started/
- התיעוד הרשמי של Gleam: https://gleam.run/core_api/index.html
- קוד המקור של Gleam ב-GitHub: https://github.com/gleam-lang/gleam