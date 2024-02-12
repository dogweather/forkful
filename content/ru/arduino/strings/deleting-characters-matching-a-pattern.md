---
title:                "Удаление символов, соответствующих шаблону"
aliases:
- /ru/arduino/deleting-characters-matching-a-pattern/
date:                  2024-01-28T23:57:30.146933-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление символов, соответствующих шаблону"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Удаление символов, соответствующих заданному шаблону, означает избавление от определённых последовательностей символов в строках — можно думать об этом как о очистке данных или ввода. Программисты делают это для стандартизации, упрощения или проверки информации перед её обработкой.

## Как это делать:

Допустим, мы хотим удалить все цифровые символы из нашей строки. У нас есть строка со случайными числами, и мы хотим получить в результате чистую строку, содержащую только буквы.

```Arduino
void setup() {
  Serial.begin(9600);

  // Наша начальная строка с числами
  String stringWithNumbers = "Ar3du1n0 1s aw3som3!";
  String cleanedString = deletePattern(stringWithNumbers, "0123456789");

  // Печатаем очищенную строку
  Serial.println(cleanedString);
}

void loop() {
  // Здесь делать нечего
}

String deletePattern(String str, String pattern) {
  for (unsigned int i = 0; i < pattern.length(); i++) {
    str.replace(String(pattern[i]), "");
  }
  return str;
}
```

Если вы загрузите и запустите это на вашем Arduino, вы увидите строку без чисел в мониторе последовательного порта:

```
Arduino is awesome!
```

## Подробнее

Удаление символов, соответствующих определённому шаблону, не является новой концепцией. В ранних языках программирования были функции для обработки и манипуляции строками. В Arduino, хотя функция высокого уровня для удаления по шаблону отсутствует встроенно, мы можем создать нашу собственную логику, как в функции `deletePattern` выше.

Существуют альтернативы на других языках, такие как регулярные выражения (regex) в Python или JavaScript, но среда программирования Arduino более базовая. Она не включает функции regex из коробки, в основном из-за ограниченной вычислительной мощности и памяти.

Под капотом наша функция `deletePattern` итерируется через нашу строку шаблона, использует метод `String.replace()` для поиска текущего символа и заменяет его на пустую строку, таким образом "удаляя" его из нашей исходной строки.

## См. также

- Манипуляции со строками в Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
- Справочник по строкам Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Больше о замене строк: http://www.cplusplus.com/reference/string/string/replace/
