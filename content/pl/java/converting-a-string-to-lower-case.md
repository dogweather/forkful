---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego? (What & Why?)
W Javie konwersja łańcucha na małe litery oznacza zmianę wszystkich dużych liter znajdujących się w łańcuchu na ich małe odpowiedniki. Programiści robią to, by ułatwić porównywanie i sortowanie łańcuchów.

## Jak to zrobić (How To)
Klasa ``String`` w Javie udostępnia metodę ``toLowerCase()``. Oto przykład jej użycia:

```Java
String str = "Hello World!";
String lowerCaseStr = str.toLowerCase();
System.out.println(lowerCaseStr);
```

To wydrukuje:

```
hello world!
```

## Dogłębna Analiza (Deep Dive)
Konwersja łańcucha na małe litery nie jest czymś znanym tylko z Javy. Jest to powtarzalny wzorzec w wielu językach programowania i ma korzenie w łatwiejszym porównywaniu łańcuchów.

Alternatywą dla metody ``toLowerCase()`` jest używanie strumieni i operacji ``map`` w Javie 8 i wyższych. Tak wygląda ten sposób:

```Java
String str = "Hello World!";
String lowerCaseStr = str.chars()
        .mapToObj(c -> Character.toLowerCase((char)c))
        .map(String::valueOf)
        .collect(Collectors.joining());
System.out.println(lowerCaseStr);
```

Metoda ``toLowerCase()`` korzysta z konfiguracji regionalnej domyślnej w systemie. Możemy też podać konkretną konfigurację regionalną, jeśli chcemy uniknąć możliwych problemów z różnicami w ustawieniach:

```Java
String str = "HELLO WORLD!";
String lowerCaseStr = str.toLowerCase(Locale.ROOT);
System.out.println(lowerCaseStr);
```

## Zobacz także (See Also)
- [Dokumentacja metody toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Tutorial Oracle o łańcuchach](https://docs.oracle.com/javase/tutorial/java/data/strings.html)