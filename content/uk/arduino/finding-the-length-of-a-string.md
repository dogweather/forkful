---
title:                "Визначення довжини рядка"
aliases:
- uk/arduino/finding-the-length-of-a-string.md
date:                  2024-01-20T17:46:52.737507-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке довжина рядка та чому це важливо? Довжина рядка – це кількість символів у ньому. Програмісти використовують цю інформацію для обробки текстових даних, валідації вводу та керування пам'яттю.

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
