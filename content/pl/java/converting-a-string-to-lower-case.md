---
title:                "Java: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów w Javie, musisz manipulować tekstem w różne sposoby. Jedną z czynności, które możesz wykonać, jest konwersja tekstu na małe litery. W tym artykule dowiesz się, dlaczego taki zabieg może być przydatny w Twoim kodzie.

## Jak to zrobić

Konwersja tekstu na małe litery w Javie jest bardzo prosta dzięki metodzie `toLowerCase()`. Wystarczy przekazać do niej jako argument funkcji ciąg znaków, którego chcesz dokonać konwersji. Poniżej znajduje się przykładowy kod:

```java
String text = "PRZYKŁADOWY TEKST";
String convertedText = text.toLowerCase();

System.out.println(convertedText);
```

W wyniku wykonania powyższego kodu otrzymamy następujący output: `przykładowy tekst`.

## Deep Dive

Głównym powodem konwersji tekstu na małe litery jest ujednolicenie danych. W przypadku porównywania tekstu, nie jest istotne, czy literki są wielkie czy małe, ale to, czy dokładnie odpowiadają sobie w kolejności. Dzięki konwersji tekstu na małe litery, mamy pewność, że porównując dwa wyrazy, nie zostanie pominięty żaden z nich ze względu na wielkość liter.

Warto również zauważyć, że metoda `toLowerCase()` będzie działać prawidłowo tylko dla standardowych znaków alfabetu angielskiego. Jeśli będziemy chcieli skonwertować tekst zawierający znaki specjalne lub litery z innych języków, musimy użyć innych metod lub rozszerzyć nasz kod.

## Zobacz również

- [Java String class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java String toLowerCase() method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)