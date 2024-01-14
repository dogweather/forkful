---
title:    "Java: З'єднання рядків"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Чому

Існує декілька сценаріїв, коли необхідно об'єднати дві або більше рядків в один в Java. Наприклад, це може знадобитися для створення повідомлень, логів або форматування виводу.

## Як

Для об'єднання рядків в Java використовується оператор з'єднання (+) або метод `concat()`. Приклади коду та виводу:

```Java
// Використання оператора +:
String hello = "Привіт";
String world = "світ!";
System.out.println(hello + " " + world);

// Використання методу concat():
String string1 = "Це";
String string2 = "рядки";
String string3 = " будуть об'єднані.";
System.out.println(string1.concat(" ").concat(string2).concat(string3));
```

Вивід:

```Java
Привіт світ!
Це рядки будуть об'єднані.
```

## Глибокий занурення

Якщо об'єднувати дуже багато рядків, краще використовувати клас `StringBuilder`, оскільки він більш ефективно працює з пам'яттю та забезпечує оптимальну швидкість виконання. Наприклад:

```Java
StringBuilder sb = new StringBuilder();

for (int i = 0; i < 10000; i++) {
    sb.append("рядок").append(i);
}

String result = sb.toString();
System.out.println(result);
```

Вивід буде містити 10000 рядків "рядокX", де X - числа від 0 до 9999.

## Дивіться також

- [Офіційна документація Java про оператор з'єднання(+)](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html)
- [Офіційна документація Java про метод concat()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#concat-java.lang.String-)

Можливість об'єднувати рядки є корисною та часто використовуваною функцією в програмуванні на Java. Сподіваємося, ця стаття допомогла вам зрозуміти, як це зробити ефективно та правильно. Щасливого програмування!