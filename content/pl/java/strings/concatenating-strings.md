---
title:                "Łączenie łańcuchów znaków"
aliases:
- /pl/java/concatenating-strings/
date:                  2024-01-20T17:35:07.509148-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Łączenie łańcuchów znaków, czyli stringów, to po prostu proces ich zestawiania w jeden ciągły tekst. Dzięki temu możemy tworzyć wiadomości, które mają sens, generować dynamiczne treści czy też manipulować danymi dla formatowania wyjścia.

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
