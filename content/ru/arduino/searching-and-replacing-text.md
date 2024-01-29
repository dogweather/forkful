---
title:                "Поиск и замена текста"
date:                  2024-01-29T00:02:12.897262-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Поиск и замена текста позволяют найти определенные символы или строки в тексте и заменить их чем-то другим. Программисты делают это для модификации кода, данных или пользовательских вводов без лишних хлопот.

## Как это сделать:

Arduino изначально не поддерживает поиск и замену строк так, как это делают высокоуровневые языки. Однако, вы можете работать с массивами символов или использовать класс `String`, который предлагает метод `replace()`. Хотя первый вариант экономит память, второй более прост в использовании. Давайте сосредоточимся на классе `String` для ясности.

```Arduino
void setup() {
  Serial.begin(9600);
  String text = "I like apples and apples are great!";
  text.replace("apples", "oranges");
  Serial.println(text);
}

void loop() {
  // Здесь делать нечего.
}
```

Пример вывода:
```
I like oranges and oranges are great!
```

## Подробнее

В прошлом задачи манипуляции со строками на микроконтроллерах были редкостью — память была ограничена, а приложения проще. В наши дни, благодаря сложным проектам и достаточному объему памяти (благодаря прогрессу в технологии микроконтроллеров), такие утилиты стали достаточно стандартными.

Если вы не хотите использовать класс `String` из-за его динамического использования памяти, которое может вызвать фрагментацию, вы все еще можете искать и заменять в строках стиля C (массивах символов, заканчивающихся нулем) с помощью функций вроде `strchr()`, `strstr()`, и ручного копирования или замены с помощью циклов. Это более сложно, но дает вам больший контроль над памятью.

Например, альтернативный способ замены подстроки может выглядеть так:

```Arduino
void replaceSubstring(char *input, const char *search, const char *replace) {
  char buffer[100];
  char *p;

  // 'strstr' проверяет, является ли 'search' частью 'input'.
  if (!(p = strstr(input, search))) return;

  // Копируем до точки, где найден 'search'.
  strncpy(buffer, input, p - input);
  buffer[p - input] = '\0';

  // Добавляем 'replace' и остальную часть 'input' после 'search'.
  sprintf(buffer+(p - input), "%s%s", replace, p + strlen(search));

  // Выводим результат
  strcpy(input, buffer);
}

void setup() {
  Serial.begin(9600);
  char text[] = "I like apples and apples are great!";
  replaceSubstring(text, "apples", "oranges");
  Serial.println(text);
}

void loop() {
  // Здесь по-прежнему делать нечего.
}
```

Пример вывода:
```
I like oranges and oranges are great!
```

## См. также

- [Arduino Справочник: Объект String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Справочник: Функция замены String](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Cplusplus.com: Функции строки C](http://www.cplusplus.com/reference/cstring/)
