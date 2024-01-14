---
title:                "Java: Wyszukiwanie i zamiana tekstu."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek znalazłeś się w sytuacji, gdzie musiałeś zmienić fragment tekstu w swoim kodzie lub dokumencie? Może zmieniła się nazwa zmiennej lub musisz dostosować tekst do innych wymagań. W takich sytuacjach bardzo pomocne jest wyszukiwanie i zamienianie tekstu. Pozwala ono szybko i skutecznie dokonać zmian bez konieczności ręcznego przechodzenia przez każdą linię tekstu. W tym wpisie dowiesz się jak wykonać to zadanie w języku Java.

## Jak to zrobić

Pierwszym krokiem jest wybranie odpowiedniej metody do wyszukiwania i zamiany tekstu. W przypadku stringów w Javie, możemy skorzystać z metody `replace()` lub `replaceAll()`. Pierwsza służy do zamiany pojedynczych znaków, natomiast druga do zamiany wyrażeń regularnych.

Przykładowo, jeśli mamy zmienną `text` zawierającą tekst "Hello world!", możemy użyć metody `replace()` aby zamienić "world" na "Java". W tym celu należy użyć następującej składni:

```Java
String newText = text.replace("world", "Java");
System.out.println(newText);
```

Po uruchomieniu powyższego kodu, otrzymamy wyjście "Hello Java!".

Jeśli chcemy wykonać zamianę na podstawie wyrażenia regularnego, możemy skorzystać z metody `replaceAll()` wraz z odpowiednim wyrażeniem.

```Java
String newText = text.replaceAll("\\d+", "Java");
System.out.println(newText);
```

W powyższym przykładzie wyrażenie regularne `\\d+` oznacza każdą liczbę występującą w tekście. W wyniku otrzymamy "Hello Java!".

## Deep Dive

W przypadku wyrażeń regularnych, możemy skorzystać także z metody `replaceFirst()` do zamiany tylko pierwszego wystąpienia wyrażenia. Dodatkowo, metoda `replace()` może przyjmować też obiekt typu `CharSequence`, co oznacza, że można użyć jej do zamiany tekstu w obiektach innych niż String.

Jednym z bardziej zaawansowanych sposobów wyszukiwania i zamiany tekstu jest użycie klas `Pattern` i `Matcher` z pakietu `java.util.regex`. Dzięki nim możemy precyzyjniej definiować wyrażenia regularne oraz manipulować tekstem za pomocą grup.

## Zobacz także

- https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-
- https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
- https://docs.oracle.com/javase/8/docs/api/java/util/regex/Matcher.html