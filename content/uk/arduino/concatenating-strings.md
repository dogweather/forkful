---
title:    "Arduino: З'єднання рядків"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Чому

Використання конкатенації рядків є важливою технікою, коли мова йде про програмування Arduino. Вона дозволяє поєднати кілька рядків у один, що полегшує роботу зі рядками даних і поліпшує швидкість виконання програми.

## Як це зробити

```Arduino
String str1 = "Привіт,";
String str2 = "Світ!";
String result = str1 + " " + str2;
Serial.println(result);
```

Результат:

```
Привіт, Світ!
```

Передбачування для цього прикладу:

- Для з'єднання рядків використовується оператор `+`.
- Знаки об'єднуються без пробілу за допомогою рядків, які вказуються в подвійних лапках (`"`).

Ви також можете додати більше рядків, як це показано у наступному прикладі:

```Arduino
String str1 = "Сьогодні";
String str2 = "хороший";
String str3 = "день.";
String result = str1 + " " + str2 + " " + str3;
Serial.println(result);
```

Результат:

```
Сьогодні хороший день.
```

Приклад поєднання рядка і числа:

```Arduino
String str = "Ви народилися у";
int year = 1990;
String result = str + " " + String(year);
Serial.println(result);
```

Результат:

```
Ви народилися у 1990
```

## Глибоке погруження

Конкатенація має деякі обмеження, які варто враховувати:

- Ця техніка працює лише з об'єктом `String`, а не з звичайними масивами символів. Тому, якщо ви хочете з'єднати багато рядків, краще користуватися бібліотекою `String`.
- Використовуючи конкатенацію, ви можете швидко вичерпати пам'ять. Тому звертайте увагу на те, щоб не перевантажувати програму багатьма великими рядками.
- Конкатенація може бути повільнішою від звичайного з'єднання рядків. Це пов'язано з тим, що під час конкатенації потрібно кожен раз створювати новий об'єкт `String`.

## Дивись також

- [Офіційна документація Arduino про розробку з рядками](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Огляд бібліотеки `String`](https://www.arduino.cc/en/Reference/StringConstructor)