---
title:                "Организация кода в функции"
date:                  2024-01-29T00:00:17.450412-07:00
model:                 gpt-4-0125-preview
simple_title:         "Организация кода в функции"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Организация кода в функции означает разделение вашего кода на повторно используемые блоки, каждый из которых выполняет определённую задачу. Программисты делают это, чтобы код был проще для чтения, отладки и повторного использования. Это как сортировка Лего по коробкам - это избавляет вас от необходимости рыться в хаотичной куче каждый раз, когда вы хотите что-то построить.

## Как это сделать:
Представьте, что вы хотите заставить светодиод мигать. Без функций ваш `loop` - это беспорядочная куча. С функциями всё аккуратно. Вот как:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Мигание светодиодом каждые 500ms
}

// Функция для мигания светодиодом
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Пример вывода: Ваш светодиод счастливо мигает, и назначение кода очевидно с первого взгляда.

## Погружение в детали
До функций программирование было линейным путешествием; вы видели каждую яму от начала до конца. После функций - это больше похоже на перелёты, когда вы перескакиваете на важные части. Исторически, подпрограммы (ранние функции) были революцией в программировании, позволяя кодерам избегать повторений – это принцип DRY, Не Повторяй Себя. Альтернативы функциям могут включать в себя макросы или использование классов для объектно-ориентированного программирования (OOP). Суть в том? Когда вы определяете функцию, вы даёте компилятору план выполнения задачи. С Arduino вы часто определяете функции типа void, которые действуют как простые команды для микроконтроллера, но функции также могут возвращать значения, что делает их более универсальными.

## Смотрите также
Для получения дополнительной информации о функциях просмотрите следующее:

- Официальная справка по функциям Arduino: https://www.arduino.cc/reference/en/language/functions/
- Узнать больше о принципе DRY: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- Обновление знаний об истории подпрограмм: https://en.wikipedia.org/wiki/Subroutine