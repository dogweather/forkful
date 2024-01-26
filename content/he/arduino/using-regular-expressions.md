---
title:                "שימוש בביטויים רגולריים"
html_title:           "Arduino: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
משתמשים בביטויים רגולריים לחיפוש והחלפת טקסט לפי תבניות. זה עוזר לפרוסס מידע מהר וביעילות.

## How to: (איך עושים את זה?)
ב-Arduino לא מובנה תמיכה בביטויים רגולריים, אבל אפשר להשתמש בספרייה כמו `Regexp`. דוגמה:

```Arduino
#include <Regexp.h>

void setup() {
  Serial.begin(9600);

  MatchState ms;
  ms.Target("Arduino123!");
  
  char result = ms.Match("%a+");

  if (result == REGEXP_MATCHED) {
    Serial.println("Match found!");
  } else {
    Serial.println("No match found.");
  }
}

void loop() {
  // פה לא צריך כלום. הכל ב-setup.
}
```
פלט:
```
Match found!
```

## Deep Dive (צלילה לעומק)
ביטויים רגולריים לא תמיד נתמכים בכל שפות או פלטפורמות. ב-Arduino, צריך ספרייה נוספת. הם הומצאו בשנות ה-50 ומשמשים ברוב שפות התכנות המודרניות. אלטרנטיבות לביטויים רגולריים כוללות חיפוש פשוט יותר עם `indexOf`, `lastIndexOf`, `startsWith`, ו-`endsWith` שכבר מובנות ב-Arduino.

## See Also (ראה גם)
- Regexp library on GitHub: https://github.com/nickgammon/Regexp
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Arduino Forum – Using regular expressions: https://forum.arduino.cc/index.php?topic=393655.0
