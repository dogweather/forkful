---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Регулярні вирази – це шаблони, щоб знайти та керувати текстом. Програмісти використовують їх для пошуку, валідації та заміни даних.

## Як це зробити:
Arduino не має вбудованої підтримки регулярних виразів, але можна використати бібліотеки сторонніх розробників.

```Arduino
#include <regex.h>

void setup() {
  Serial.begin(9600);
  regex_t reg;
  const char * regex_text = "Arduino";
  const char * find_text = "I love Arduino boards!";

  if (regcomp(&reg, regex_text, REG_EXTENDED) == 0) {
    regmatch_t matches[MAX_MATCHES];
    if (regexec(&reg, find_text, MAX_MATCHES, matches, 0) == 0) {
      Serial.println("Match found!");
    } else {
      Serial.println("No match found.");
    }
  }
  regfree(&reg);
}

void loop() {
  // Nothing to do here
}
```

Sample output:
```
Match found!
```

## Поглиблений огляд:
Регулярні вирази з'явились у 1950-х, до Arduino часів. Без прямої підтримки, в Arduino можна використовувати бібліотеки `regex.h` або аналоги. Важливо знати: регулярні вирази вимагають більше пам'яті.

## Дивись також:
- [Arduino Regex Library by Nick Gammon](http://www.gammon.com.au/forum/?id=11063)
- [Matching Strings with Regular Expressions in C++](https://www.cplusplus.com/reference/regex/)
