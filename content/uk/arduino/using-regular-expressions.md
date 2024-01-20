---
title:                "Використання регулярних виразів"
html_title:           "Arduino: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що і чому?

Регулярні вирази, або RegExp, це потужний інструмент для пошуку і маніпуляцій з текстом у коді. Програмісти використовують їх для ефективного виявлення, витягування та заміни певних шаблонів у стрічках.

## Як це працює:

Нижче наведено приклад пошуку стрічки, що містить слово "Arduino" за допомогою регулярних виразів.

```Arduino
#include <regex.h>
String input = "I love Arduino";
regex_t regex;
int check;

check = regcomp(&regex, "Arduino", 0);
check = regexec(&regex, input.c_str(), 0, NULL, 0);

if(!check) {
  Serial.println("Match found");
} else if(check == REG_NOMATCH) {
  Serial.println("No match");
} else {
  regerror(check, &regex, input, sizeof(input));
  Serial.println("Regex match failed");
}
```

Виведення:

```Arduino
"Match found"
```

## Глибше занурення

1. Регулярні вирази були вперше впроваджені в 1951 році в теорію формальних мов та автоматів. Вони стали основою багатьох сучасних мов програмування включно з Arduino.

2. Альтернативами RegExp є методи пошуку та заміни вбудованих бібліотек стрічок, але вони не надають такої гнучкості та точності.

3. Arduino підтримує більшість стандартних синтаксисів RegExp, але можуть бути виключення в залежності від версії та бібліотеки.

## Дивись також

3. [Довідник по регулярним виразам](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)