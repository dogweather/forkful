---
title:    "Ruby: Znajdowanie długości ciągu znaków"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego?

Jedną z podstawowych umiejętności w programowaniu jest operowanie na ciągach znaków. Często zdarza się, że potrzebujemy określić długość danego tekstu lub wykorzystać jego długość w innym celu. W takiej sytuacji niezbędne jest poznanie sposobów na znalezienie długości napisu w języku Ruby.

## Jak to zrobić?

Sprawdzenie długości napisu w Ruby jest bardzo proste. Wystarczy wykorzystać metodę `length` na zmiennej będącej tekstem, który chcemy zbadać. Poniżej znajduje się przykładowy kod oraz wynik wywołania tej metody:

```Ruby
tekst = "Cześć, czy to jest ciąg znaków?"
puts tekst.length # wynik: 28
```

W powyższym przykładzie zmienna `tekst` przechowuje dany napis, a następnie wywołujemy na niej metodę `length`, która zwraca długość tego napisu. Jest to bardzo wygodny i szybki sposób na uzyskanie tej informacji.

## Głębsze zagadnienia

Podczas korzystania z metody `length` warto pamiętać, że w języku Ruby wszystko jest obiektem, również ciągi znaków. Dlatego też, na zmiennej z tekstem możemy wywoływać nie tylko metodę `length`, ale również inne metody dostępne dla obiektów typu `String`.

Ponadto, istnieje możliwość wykorzystania symbole `#{}`, aby wewnątrz napisu umieścić wyrażenie, które zostanie automatycznie przekonwertowane na ciąg znaków. Przykładowo:

```Ruby
tekst = "Długość tego napisu to #{tekst.length} znaków."
puts tekst # wynik: Długość tego napisu to 28 znaków.
```

## Zobacz także

- Dokumentacja Ruby o metodzie `length`: https://ruby-doc.org/core-2.7.2/String.html#method-i-length
- Wideo tutorial na temat ciągów znaków w Ruby: https://www.youtube.com/watch?v=9mQUZOKPM8U
- Wstępny kurs programowania w Ruby: https://rubylearning.com/satishtalim/tutorial.html