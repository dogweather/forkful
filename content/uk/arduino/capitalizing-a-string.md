---
title:                "Приведення рядка до великої букви"
html_title:           "Arduino: Приведення рядка до великої букви"
simple_title:         "Приведення рядка до великої букви"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо це потрібно?

Капіталізація рядка - це перетворення першого символу цього рядка на велику букву. Програмісти роблять це для того, щоб покращити читабельність та оформлення тексту в коді.

## Як це зробити:

На жаль, в Arduino немає вбудованої функції для капіталізації рядків. Однак це не проблема - ми можемо створити власну функцію, яка це робить:

```Arduino
String capitalize(String str) {
  if (str.length() > 0) { 
       str[0] = toupper(str[0]); 
  }  
  return str; 
}

void setup() {
  String myString = "hello world!";
  Serial.begin(9600);
  Serial.println(capitalize(myString)); 
  // Outputs: "Hello world!"
}

void loop() {}
```
На виході ми отримуємо: "Hello world!"

## Занурення у деталі:

Капіталізація рядків була важливою операцією з часів, коли люди вперше почали використовувати комп'ютери для обробки тексту. Ця проста акція може значно покращити зрозумілість тексту, коли відповідно використовується. Однак, існує кілька альтернатив капіталізації, включаючи переведення всього тексту у верхній регістр або нижній регістр.

## Дивись також:

- [Офіційна документація до Arduino](https://www.arduino.cc/reference/pl/)
- [Розділ Arduino про рядки](https://www.arduino.cc/reference/en/language/variables/data-types/stringfunctions/)
- [Детальніше про функцію toupper()](http://www.cplusplus.com/reference/cctype/toupper/)