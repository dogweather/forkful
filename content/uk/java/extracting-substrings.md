---
title:    "Java: Видобування підрядків"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Чому

Якщо ви працюєте з рядками в Java, шанси є, що ви матимете потребу вибирати підрядки зі своїх рядків. Це корисна техніка для роботи зі словниками, регулярними виразами і багатьма іншими проблемами. Таким чином, вивчення того, як витягти підрядки в Java, може бути цінною навичкою для будь-якого програміста.

## Як це зробити

Для початку, вам потрібно обрати рядок, з якого ви хочете витягти підрядок. Для цього ви можете використовувати метод `substring()` і передати йому початковий та кінцевий індекси вашого підрядка. Наприклад:

```Java
String str = "Привіт, Україно!";
String substring = str.substring(7, 15);
System.out.println(substring);
```

Вивід буде: `Україно!`

Крім того, ви можете використовувати метод `substring()` з одним параметром, щоб витягти підрядок з початкового індексу до кінця рядка:

```Java
String str = "Привіт, Україно!";
String substring = str.substring(7);
System.out.println(substring);
```

Вивід буде: `Україно!`

Також, ви можете використовувати метод `indexOf()` для пошуку індексу певного символу у рядку і передати його як початковий або кінцевий індекс у метод `substring()`. Наприклад, ви можете витягти підрядок між двома комами наступним чином:

```Java
String str = "Привіт, Україно!";
int firstCommaIndex = str.indexOf(",");
int secondCommaIndex = str.indexOf(",", firstCommaIndex + 1);
String substring = str.substring(firstCommaIndex + 2, secondCommaIndex);
System.out.println(substring);
```

Вивід буде: `Україно`

## Глибокий занурення

Крім вищезазначених базових методів, в Java є також клас `StringTokenizer`, який дозволяє вам швидше і зручніше витягти підрядок з рядка за допомогою роздільника. Ось приклад використання цього класу:

```Java
String str = "Привіт, Україно!";
StringTokenizer st = new StringTokenizer(str, ",");
String substring = "";
while(st.hasMoreTokens()) {
  substring = st.nextToken();
}
System.out.println(substring);
```

Вивід буде: `Україно!`

## Дивіться також

- [Документація Java String `substring()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Документація Java String `indexOf()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#indexOf-java.lang.String-)
- [Документація Java `StringTokenizer`](https://docs.oracle.com/javase/8/docs/api/java/util/StringTokenizer.html)