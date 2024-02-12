---
title:                "Рефакторинг"
date:                  2024-01-29T00:03:22.987883-07:00
model:                 gpt-4-0125-preview
simple_title:         "Рефакторинг"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/refactoring.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Рефакторинг — это процесс переработки вашего кода для улучшения его структуры и читаемости, не изменяя внешнее поведение или функциональность. Программисты используют рефакторинг, чтобы сделать свой код более чистым, понятным и легко поддерживаемым, что в долгосрочной перспективе значительно упрощает отладку и добавление новых функций.

## Как:

Допустим, у вас есть функция на вашем Arduino, которая делает слишком много, вот так:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Функция, которая делает слишком много
  handleEverything();
}

void handleEverything() {
  // Чтение данных сенсора
  int sensorValue = analogRead(A0);
  // Обработка данных сенсора
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // Вывод данных сенсора
  Serial.println(sensorValue);
  delay(500);
}
```

Рефакторинг может выглядеть как разделение `handleEverything()` на более мелкие, сфокусированные функции:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

После рефакторинга функция `loop()` становится более читаемой, и каждая задача обрабатывается отдельной функцией, что делает код более легко управляемым.

## Углубление
Исторически рефакторинг стал популярен с ростом методологий Agile и разработки через тестирование (TDD), которые опираются на постоянное улучшение кода для адаптации к изменяющимся требованиям. Существуют различные инструменты и стратегии для рефакторинга — например, техника "Extract Method", которую мы использовали в нашем примере с Arduino. Это особенно важно, когда вы переходите от быстрого прототипа к стабильному проекту, где читаемость кода и его поддержка становятся критически важными.

При рефакторинге важно иметь хороший набор тестов, чтобы убедиться, что изменения не внесли никаких ошибок. В мире Arduino автоматизированное тестирование не всегда просто из-за зависимостей от аппаратного обеспечения, но вы все равно можете использовать модульное тестирование для чисто логических частей или применять симуляторы.

Альтернативы ручному рефакторингу включают в себя использование специализированных инструментов для рефакторинга, которые автоматизируют идентификацию "запахов" кода и предлагают изменения. Однако эти инструменты часто не учитывают нюансы кода для микроконтроллеров и могут быть недоступны в среде разработки Arduino.

В конечном итоге, рефакторинг — это искусство, балансирующее между улучшением внутренней структуры кода и риском внесения дефектов. Он требует от вас размышлений о деталях реализации, таких как использование памяти и процессорное время, особенно из-за ограниченных ресурсов микроконтроллеров.

## Смотрите также
Вы можете глубже погрузиться в рефакторинг с книгой Мартина Фаулера *Рефакторинг: Улучшение дизайна существующего кода*. Для ближайшего рассмотрения практик, специфичных для Arduino, обратите внимание на форумы и сообщества по разработке на Arduino:

- [Форум Arduino - Вопросы по программированию](https://forum.arduino.cc/index.php?board=4.0)
- [Refactoring Guru](https://refactoring.guru/refactoring)

Помните, цель - чистый, понятный код, за который вас и других в будущем будут благодарны. Продолжайте хакать и держите код в чистоте!