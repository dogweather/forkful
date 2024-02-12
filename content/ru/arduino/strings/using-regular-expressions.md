---
title:                "Использование регулярных выражений"
aliases:
- /ru/arduino/using-regular-expressions/
date:                  2024-01-29T00:03:38.171874-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Регулярные выражения (regex) позволяют искать текст по шаблонам — представьте себе улучшенный символ подстановки. Программисты используют их для проверки ввода, поиска строк и эффективного извлечения данных.

## Как:

Arduino не имеет встроенной поддержки regex, но вы можете имитировать простые проверки шаблонов. Для более сложных задач рассмотрите возможность использования библиотеки regex, такой как `Regexp`.

```Arduino
#include <Regexp.h>

void setup() {
  Serial.begin(9600);
  
  MatchState ms;
  char result;
  
  ms.Target ("Привет, мир!");
  result = ms.Match ("(мир)");

  if (result > 0) {
    char captured[10]; // Убедитесь, что это достаточно большой размер для вашего совпадения
    ms.GetCapture (captured, 0);
    Serial.print("Совпадение найдено: ");
    Serial.println(captured);
  } else {
    Serial.println("Совпадений не найдено.");
  }
}

void loop() {
  // Здесь делать нечего.
}
```

Пример вывода:
```
Совпадение найдено: мир
```

## Глубже

Регулярные выражения происходят из теоретической информатики и существуют с 1950-х годов. Perl и другие языки имеют сильную реализацию regex, но на Arduino ресурсы ограничены, поэтому нет встроенной поддержки. Библиотеки вроде `Regexp` могут стать вашим спасением — они берут на себя часть нагрузки, но помните, что они могут быть ресурсоемкими для меньших микроконтроллеров.

## Смотрите также

Проверьте эти ссылки для получения дополнительной информации:

- Библиотека `Regexp` для Arduino: [https://www.arduino.cc/reference/en/libraries/regexp/](https://www.arduino.cc/reference/en/libraries/regexp/)
- Репозиторий библиотеки `Regexp` на GitHub: [https://github.com/nickgammon/Regexp](https://github.com/nickgammon/Regexp)
- Онлайн тестер regex (для разработки вашего regex перед реализацией): [https://regexr.com/](https://regexr.com/)
