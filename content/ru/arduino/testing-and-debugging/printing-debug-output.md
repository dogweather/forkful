---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:34.510794-07:00
description: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\
  \u043D\u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438\
  \ \u043D\u0430 \u0441\u0435\u0440\u0438\u0439\u043D\u044B\u0439 \u043C\u043E\u043D\
  \u0438\u0442\u043E\u0440 - \u044D\u0442\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  \ \u0437\u0430\u0433\u043B\u044F\u043D\u0443\u0442\u044C \u0432 \u0443\u043C Arduino.\
  \ \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\
  \u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E, \u0447\u0442\u043E\u0431\u044B\
  \ \u043E\u0442\u0441\u043B\u0435\u0434\u0438\u0442\u044C \u043E\u0448\u0438\u0431\
  \u043A\u0438, \u043F\u0440\u043E\u0432\u0435\u0440\u0438\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:45.534871-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\
  \u043D\u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438\
  \ \u043D\u0430 \u0441\u0435\u0440\u0438\u0439\u043D\u044B\u0439 \u043C\u043E\u043D\
  \u0438\u0442\u043E\u0440 - \u044D\u0442\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  \ \u0437\u0430\u0433\u043B\u044F\u043D\u0443\u0442\u044C \u0432 \u0443\u043C Arduino.\
  \ \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\
  \u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E, \u0447\u0442\u043E\u0431\u044B\
  \ \u043E\u0442\u0441\u043B\u0435\u0434\u0438\u0442\u044C \u043E\u0448\u0438\u0431\
  \u043A\u0438, \u043F\u0440\u043E\u0432\u0435\u0440\u0438\u0442\u044C\u2026"
title: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\u043D\
  \u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438"
---

{{< edit_this_page >}}

## Что и почему?

Вывод отладочной информации на серийный монитор - это способ заглянуть в ум Arduino. Программисты делают это, чтобы отследить ошибки, проверить предположения и наблюдать за данными в реальном времени, не прибегая к сложным методам отладки.

## Как это сделать:

Давайте перейдем к сути. Допустим, вы хотите выводить "Привет, мир!" каждую секунду. Вот фрагмент кода:

```Arduino
void setup() {
  Serial.begin(9600);  // Начать серийное общение
}

void loop() {
  Serial.println("Привет, мир!");  // Вывести сообщение
  delay(1000);  // Подождать секунду
}
```

Запустите Серийный Монитор в Arduino IDE и наблюдайте, как слова сыпятся, словно по расписанию. Пример вывода:

```
Привет, мир!
Привет, мир!
Привет, мир!
...
```

## Глубокое погружение

До того как `Serial` стал нашим верным помощником, люди использовали мигающие светодиоды для общения - это был каменный век отладки. Затем появилось серьезное оборудование для отладки, но оно было дорогостоящим. `Serial.print()` и его родственные функции теперь позволяют нам бросать тексты на экран с поразительной скоростью, дешево и сердито.

Альтернативы? Ну, у вас есть ЖК-дисплеи, запись на SD-карты, даже Bluetooth для тех, кто не любит провода. У каждого метода есть свои особенности; `Serial` просто стреляет прямо - просто, непосредственно, всегда под рукой.

Внутри `Serial.print()` преобразует ваши данные в байты, которые мчатся по USB к вашему компьютеру. Это происходит через аппаратные (UART) или программно эмулированные (SoftSerial) серийные порты. Это надежно, но засорение порта слишком большим объемом данных может затруднить поток вашей программы, поэтому используйте серийные печати, как будто вы приправляете стейк, а не заливаете суп.

## Смотрите также

Для тех, кто хочет узнать больше:

- Руководство по `Serial` от Arduino: [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- Наука о серийной связи: [UART Communication](https://www.sparkfun.com/tutorials/215)
