---
title:                "Сравнение двух дат"
date:                  2024-01-28T23:56:06.785009-07:00
model:                 gpt-4-0125-preview
simple_title:         "Сравнение двух дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Сравнение двух дат означает выяснение, какая из них раньше, позже, или они одинаковые. Программисты делают это для отслеживания событий, связанных с временем, таких как планирование задач или ведение журнала данных во времени.

## Как:
В Arduino вы можете сравнивать даты, используя библиотеку `TimeLib.h`. Сначала установите ее. Затем ознакомьтесь с этим фрагментом:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  // Задаем два разных времени (год, месяц, день, час, минута, секунда)
  // Здесь мы устанавливаем 3 марта 2023 года, 8:30:00 и 4 марта 2023 года, 16:45:00
  time_t firstTime = makeTime({0, 30, 8, 3, 3, 2023});
  time_t secondTime = makeTime({0, 45, 16, 4, 3, 2023});
  
  // Сравниваем два времени
  if (firstTime < secondTime) {
    Serial.print("Первое время раньше.");
  } else if (firstTime > secondTime) {
    Serial.print("Второе время раньше.");
  } else {
    Serial.print("Оба времени одинаковые.");
  }
}

void loop() {
  // Здесь ничего нет
}
```

Пример вывода:
```
Первое время раньше.
```

## Глубже
В Arduino нет встроенной поддержки даты и времени, поэтому мы используем такие библиотеки, как `TimeLib.h`. До появления библиотек людям приходилось вручную вычислять и сравнивать даты — сложная задача из-за високосных годов, разной длительности месяцев и т. д.

Другие способы работы с датами включают модули RTC (Real Time Clock), такие как DS3231, которые сохраняют время даже когда Arduino выключен. Для сравнения вам все равно придется загружать даты в программу и сравнивать их так же, как мы сделали выше.

При реализации учитывайте часовые пояса и переход на летнее время, если это необходимо. TimeLib может работать с временем UTC, что обходит эти вопросы, но для местного времени требуется особое внимание.

## См. Также
- [Документация библиотеки TimeLib](https://www.pjrc.com/teensy/td_libs_Time.html) - Подробности использования библиотеки Time.
- [Библиотека времени Arduino](https://github.com/PaulStoffregen/Time) - Репозиторий GitHub для библиотеки времени.