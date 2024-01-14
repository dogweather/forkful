---
title:    "Arduino: Перетворення дати в рядок."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Чому
Існує багато разів, коли працюючи з Arduino, нам потрібно отримати поточну дату чи час для використання їх у наших проєктах. Перетворення дати в рядок є одним з найзручніших способів зберігання цих значень та використання їх для подальшої обробки.

## Як
Щоб перетворити дату в рядок за допомогою Arduino, можна використовувати бібліотеку "Time". Нижче наведено приклад коду та його виведення:

```Arduino
#include <Time.h>
void setup() {
    // ініціалізуємо бібліотеку Time
    setTime(14, 36, 42, 5, 6, 2020); //год., хв., сек., день тижня, день, місяць, рік
    // створюємо змінну типу "tmElements_t"
    tmElements_t now;
    // заповнюємо її поточною датою та часом
    now = secondstimeToTm(now()); // повертає поточний час
    // перетворюємо змінну now у рядок
    String date = String(year(now)) + "/" + month(now) + "/" + day(now);
    Serial.println(date); // виведення рядка
}
```

Виведення: "2020/6/5"

## Deep Dive
Щоб краще розібратися в перетворенні дати в рядок, варто розглянути використання функції `tmElements_t`. Вона дозволяє представити дату та час у вигляді окремих елементів, таких як рік, місяць, день, години, хвилини та секунди. Для перетворення їх в рядок використовується функція `String()`, яка об'єднує кожен елемент у рядок за допомогою знака "+".

## Дивись також
- [Програмування на Arduino для початківців](https://hubbubshop.com/article/programmirovanie-arduino-dlya-nachinayushchikh/)
- [Офіційна документація бібліотеки Time](https://playground.arduino.cc/Code/time/) 
- [Перетворення рядка в дату на Arduino](https://www.arduino.cc/reference/tr/string/conversion)