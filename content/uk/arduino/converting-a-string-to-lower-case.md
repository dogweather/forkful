---
title:    "Arduino: Перетворення рядка в нижній регістр."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Чому

Навіть у програмуванні іноді потрібно працювати з текстом, наприклад, змінювати його формат чи порівнювати. Конвертування рядків до нижнього регістру є одним з таких прикладів, використовуваних у багатьох різних програмах. Тому, щоб розуміти, як це зробити в Arduino, прочитайте цю статтю.

## Як зробити

Для того, щоб конвертувати рядок до нижнього регістру в Arduino, використовуйте функцію `toLowerCase()`. Ось приклад коду та його виведення:

```Arduino 
String text = "HELLO WORLD";
Serial.println(text.toLowerCase());
```

Виведення: `hello world`

## Глибше розбираймось

Коли ми викликаємо функцію `toLowerCase()`, вона повертає копію рядка з усіма символами переведеними до нижнього регістру. Це корисно для порівняння двох рядків чи виведення тексту у вигляді, зрозумілому для користувача. Не забувайте також, що ця функція працює тільки з альфанумеричними символами, тому якщо ваш рядок містить спеціальні символи, вони залишаться без змін.

## Дивись також

- [Arduino Reference - toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Converting Uppercase String to Lowercase in Arduino](https://www.microcontroller-project.com/converting-uppercase-string-to-lowercase.html)
- [String Conversion - ASCII to Lowercase](https://forum.arduino.cc/t/string-conversion-ascii-to-lowercase-help/390520)