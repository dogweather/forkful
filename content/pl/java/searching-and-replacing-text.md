---
title:                "Java: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli często pracujesz z tekstem w swoim kodzie Java i musisz dokonywać zmian w wielu miejscach, metoda wyszukiwania i zamiany może być niezwykle przydatna. Zamiast ręcznie edytować każde wystąpienie tekstu, możesz po prostu użyć tej funkcji i zaoszczędzić sobie cennego czasu i wysiłku.

## Jak to zrobić

Aby użyć metody wyszukiwania i zamiany w swoim kodzie Java, możesz skorzystać z następującej składni:

```java
String newText = oldText.replaceAll("stary_tekst", "nowy_tekst"); 
System.out.println(newText);
```

W powyższym przykładzie, zmienna `newText` będzie zawierać zmieniony tekst, który można następnie wyświetlić na ekranie za pomocą `System.out.println()`.

## Głębsza analiza

Możesz również użyć metody wyszukiwania i zamiany do bardziej zaawansowanych operacji, takich jak wykorzystanie wyrażeń regularnych. Na przykład, jeśli chcesz zamienić wszystkie liczby w tekście na słowo "liczba", możesz użyć następującego kodu:

```java
String text = "Mam 5 samochodów i 3 rowery";
String newText = text.replaceAll("\\d+", "liczba");
System.out.println(newText);
```

Otrzymasz wtedy wyjście "Mam liczba samochodów i liczba rowery". Wyrażenia regularne pozwalają na jeszcze większą kontrolę nad procesem wyszukiwania i zamiany, ale wymagają nieco nauki i praktyki. 

## Zobacz również

- Dokumentacja Java String: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll(java.lang.String,%20java.lang.String)
- Tutorial o wyrażeniach regularnych w Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html