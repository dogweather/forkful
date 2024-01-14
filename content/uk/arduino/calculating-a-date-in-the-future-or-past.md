---
title:    "Arduino: Обчислення дати у майбутньому або минулому"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Чому

Розрахунок дати в майбутньому або минулому може бути корисним для створення розкладів, датчиків або в будь-яких проєктах, де потрібно працювати з часом.

# Як зробити

Бібліотека Time в Arduino дозволяє легко маніпулювати датою і часом. Перш ніж розпочати, вам необхідно підключити бібліотеку, використовуючи "скетч - підключити бібліотеку - Time". Після цього потрібно встановити початкову дату і час. Для цього використовується функція ` setTime (години, хвилини, секунди, день, місяць, рік) `. Тепер ми можемо отримати поточну дату і час за допомогою функцій ` now () ` та ` now (). Як приклад, розрахуємо дату, яка буде через 10 днів від заданої початкової дати:

```Arduino
#include <Time.h>

void setup() {
  setTime(15, 30, 0, 19, 9, 2021); // Встановлюємо початкову дату та час
  Serial.begin(9600);
}

void loop() {
  time_t currentTime = now(); // Отримуємо поточний час
  time_t futureTime = currentTime + (10 * SECS_PER_DAY); // Розраховуємо час через 10 днів
  tmElements_t futureDate; // Створюємо об'єкт структури для зберігання дати
  breakTime(futureTime, futureDate); // Переходимо від часу до дати

  // Виводимо результат у форматі "YY/MM/DD"
  Serial.print(futureDate.Year);
  Serial.print("/");
  Serial.print(futureDate.Month);
  Serial.print("/");
  Serial.println(futureDate.Day);

  delay(1000); // Почекаємо 1 секунду
}
```

Результат виводу буде "21/09/29" - дата, яка буде через 10 днів після 19 вересня 2021 року.

# Глибоке занурення

Бібліотека Time також має багато інших функцій для маніпулювання датою і часом, таких як `hour () `, ` minute () `, ` day () ` та багато інших. Детальніше про ці функції та їх використання можна дізнатися у документації бібліотеки.

# Дивіться також

- [Документація бібліотеки Time](https://www.arduino.cc/en/Reference/Time)
- [Приклади використання бібліотеки Time](https://www.arduino.cc/en/Tutorial/Time)
- [Стаття про роботу з датами та часом в Arduino](https://startingelectronics.org/tutorials/arduino/modules/time-date-module/)