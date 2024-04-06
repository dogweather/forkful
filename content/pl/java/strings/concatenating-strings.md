---
date: 2024-01-20 17:35:07.509148-07:00
description: "How to? (Jak to zrobi\u0107?) \u0141\u0105czenie string\xF3w w Javie\
  \ to pestka. Mo\u017Cesz u\u017Cy\u0107 operatora plus (+), metody `concat()` lub\
  \ klasy `StringBuilder`. Oto przyk\u0142ady."
lastmod: '2024-04-05T21:53:36.708891-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) \u0141\u0105czenie string\xF3w w Javie to pestka."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## How to? (Jak to zrobić?)
Łączenie stringów w Javie to pestka. Możesz użyć operatora plus (+), metody `concat()` lub klasy `StringBuilder`. Oto przykłady:

```java
// Operator +
String greeting = "Cześć, " + "jak się masz?";
System.out.println(greeting); // Wyświetli: Cześć, jak się masz?

// Metoda concat()
String hello = "Hej ".concat("świecie!");
System.out.println(hello); // Wyświetli: Hej świecie!

// StringBuilder
StringBuilder builder = new StringBuilder();
builder.append("To ").append("jest ").append("proste.");
String message = builder.toString();
System.out.println(message); // Wyświetli: To jest proste.
```

## Deep Dive (Głębsze spojrzenie)
Historia łączenia stringów sięga początków programowania. W Javie, każdy string jest obiektem klasy `String`, która jest niemutowalna. To oznacza, że każde łączenie tworzy nowy obiekt, co może być problematyczne dla wydajności.

Alternatywy jak `StringBuilder` (dla operacji w obrębie jednego wątku) i `StringBuffer` (dla operacji wielowątkowych) są bardziej efektywne. Wprowadzono je, by zapewnić wydajność przy częstym dodawaniu stringów.

Kiedy używasz `+` do łączenia stringów, kompilator w tle zamienia to na `StringBuilder.append()`. Więc dla prostych operacji może to być ok, ale przy mnóstwie łączeń w pętli lepiej użyc `StringBuilder` bezpośrednio.

## See Also (Zobacz również)
- [Dokumentacja Java – StringBuilder](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html)
- [Oracle Tutorial on Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
