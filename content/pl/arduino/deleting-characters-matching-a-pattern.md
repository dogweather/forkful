---
title:                "Arduino: Usuwanie znaków pasujących do wzorca."
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Arduino jest popularną platformą programistyczną, która umożliwia tworzenie różnorodnych projektów, takich jak roboty, systemy kontrolne czy urządzenia elektroniczne. Często w takich projektach konieczne jest przetwarzanie danych tekstowych, w tym usuwanie znaków spełniających określony wzorzec. W tym artykule dowiesz się, jak w prosty sposób wykonać taką operację w języku Arduino.

## Jak To Zrobić

Aby usunąć znaki spełniające określony wzorzec w Arduino, należy wykorzystać funkcję `String::replace()`. Przykładowy kod wyglądałby następująco:

```Arduino
String tekst = "Hello123World456";
tekst.replace("123", "");
tekst.replace("456", "");
Serial.println(tekst); // Wyświetli "HelloWorld"
```

W powyższym przykładzie najpierw tworzymy obiekt typu `String`, który przechowuje nasz tekst. Następnie używamy funkcji `replace()` dwukrotnie - najpierw usuwając znaki "123", a potem "456". Na koniec wyświetlamy zmieniony tekst za pomocą funkcji `Serial.println()`. Możesz również zastosować pętlę `for` i funkcję `replace()` w celu automatyzacji tego procesu.

## Głębsze Wprowadzenie

Funkcja `String::replace()` działa na obiekcie typu `String` i może przyjąć dwa argumenty - pierwszy to tekst, który chcemy zastąpić, a drugi to tekst, którym ma być zastąpiony. W przypadku, gdy drugi argument jest pusty, funkcja po prostu usuwa wszystkie wystąpienia pierwszego argumentu.

Warto również pamiętać, że funkcja ta jest często używana w połączeniu z innymi funkcjami, takimi jak `String::indexOf()`, które umożliwiają znalezienie pierwszego wystąpienia danego znaku lub sekwencji w tekście.

## Zobacz również

Jeśli jesteś zainteresowany dalszym zgłębianiem tematu usuwania znaków w Arduino, polecamy zapoznanie się z dokumentacją na temat funkcji `String::replace()` oraz innych użytecznych funkcji do obsługi tekstów w tej platformie:

- [Dokumentacja funkcji `String::replace()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)

- [Dokumentacja funkcji `String::indexOf()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)

- [Wideo-tutorial na temat pracy z tekstami w Arduino](https://www.youtube.com/watch?v=1PGm8LslEbE)

Dziękujemy za przeczytanie tego artykułu i mamy nadzieję, że teraz umiesz już efektywnie usuwać znaki w Arduino. W razie dodatkowych pytań lub sugestii odnośnie tematu, zapraszamy do zostawienia komentarza. Happy coding! 😊