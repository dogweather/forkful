---
title:                "Java: Usuwanie znaków odpowiadających wzorcowi"
simple_title:         "Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią naszego życia, szczególnie w świecie technologii. Czasami zdarza się, że musimy zmodyfikować nasz tekst, usuwając pewne znaki lub wzorce. W tym artykule dowiesz się, dlaczego od czasu do czasu jest to potrzebne w Twoim kodzie.

## Jak to zrobić

Jeśli chcesz usunąć znaki lub wzorce ze swojego tekstu w języku Java, istnieje kilka sposobów, w zależności od Twoich potrzeb. Oto przykładowy kod w języku Java, pokazujący dwa sposoby usuwania znaków z tekstu:

```java
// Tworzenie tekstu do modyfikacji
String text = "To jest przykładowy tekst do usunięcia znaków.";

// Usuwanie wszystkich znaków niewidocznych (np. spacji)
String newText = text.replaceAll("\\s+", "");

// Usuwanie wszystkich wystąpień wybranego znaku
String newText2 = text.replace("k", "");

// Wypisanie wyników
System.out.println(newText);
System.out.println(newText2);
```

Powyższy kod wyświetli następujące wyniki:

```
Tojestprzykładowytekstdousunięciznaków.
To jest pryadowy tekst do usunięcia znów.
```

Jak widać, używając metody `replaceAll()` lub `replace()`, możemy z łatwością usunąć wybrane znaki lub wzorce z tekstu.

## Deep Dive

Aby lepiej zrozumieć działanie powyższych metod, warto wiedzieć, że metoda `replace()` zastępuje tylko pierwsze wystąpienie danego znaku lub wzorca, natomiast `replaceAll()` jest w stanie zastąpić wszystkie wystąpienia w tekście. W obu przypadkach, kiedy nie zostanie podany drugi argument zastępowania, znak lub wzorzec będzie po prostu usuwany z tekstu.

Ważne jest również, aby zwrócić uwagę na użyty znak `\` przed znakiem `\s+` w metodzie `replaceAll()`. Ten dodatkowy znak jest niezbędny, ponieważ `\` jest używany do specjalnych znaków i musi zostać zabezpieczony przed użyciem. Podając `\\s+` informujemy program, że chcemy usunąć wszystkie znaki niewidoczne, takie jak spacje, tabulacje czy entery.

## Zobacz również

- Dokumentacja Java: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Poradnik na temat usuwania znaków z tekstu w języku Java: https://www.baeldung.com/java-string-remove-character