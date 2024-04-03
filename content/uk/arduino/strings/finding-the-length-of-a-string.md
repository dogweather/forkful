---
date: 2024-01-20 17:46:52.737507-07:00
description: "How to: \u041E\u0441\u044C \u044F\u043A \u043E\u0442\u0440\u0438\u043C\
  \u0430\u0442\u0438 \u0434\u043E\u0432\u0436\u0438\u043D\u0443 \u0440\u044F\u0434\
  \u043A\u0430 \u0432 Arduino."
lastmod: '2024-03-13T22:44:49.705366-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0441\u044C \u044F\u043A \u043E\u0442\u0440\u0438\u043C\u0430\u0442\
  \u0438 \u0434\u043E\u0432\u0436\u0438\u043D\u0443 \u0440\u044F\u0434\u043A\u0430\
  \ \u0432 Arduino."
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
weight: 7
---

## How to:
Ось як отримати довжину рядка в Arduino:

```Arduino
void setup() {
  Serial.begin(9600);
  String myString = "Привіт, світе!";
  int stringLength = myString.length();
  Serial.println(stringLength);
}

void loop() {
  // nothing to do here
}
```
**Вивід:** `14`

## Deep Dive
Ардуїно широко використовується для DIY проектів. Отже, вам часто потрібно буде працювати з текстовими рядками. Функція `length()` є базовою та швидкою. Історично в мовах програмування завжди була потреба визначити довжину рядка, щоб уникнути переповнення буферів та інших помилок. В Arduino, є інші способи, як-от `strlen()` для `char` масивів. Але `String.length()` є більш безпечним та простим у використанні.

## See Also
- [Arduino Reference for String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Forum for String Discussion ](https://forum.arduino.cc/index.php?board=9.0)
