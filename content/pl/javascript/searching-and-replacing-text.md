---
title:    "Javascript: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Podstawowym elementem programowania jest praca z tekstem. W wielu przypadkach, może się zdarzyć, że będziemy musieli zmienić lub poprawić fragment tekstu w naszym kodzie. Wówczas bardzo przydatną umiejętnością jest umiejętność wyszukiwania i zamiany tekstu. Dzięki temu, możemy szybko i skutecznie wprowadzać zmiany w naszym kodzie.

## Jak to zrobić

Do wyszukiwania i zamiany tekstu w JavaScript możemy wykorzystać metody `replace()` oraz `replaceAll()`. Pierwsza z nich służy do zamiany jednego wystąpienia danego słowa lub frazy, natomiast druga umożliwia zmianę wszystkich wystąpień w tekście.

Przykład użycia `replace()`:
```Javascript
let tekst = "Cześć, witaj na moim blogu!";
let nowyTekst = tekst.replace("witaj", "dzień dobry");
console.log(nowyTekst); // Cześć, dzień dobry na moim blogu!
```

Przykład użycia `replaceAll()`:
```Javascript
let tekst = "To jest przykład przykład przykład!";
let nowyTekst = tekst.replaceAll("przykład", "przykładem");
console.log(nowyTekst); // To jest przykładem przykładem przykładem!
```

Obie metody przyjmują dwa parametry - pierwszy to wyszukiwany fragment, a drugi to zastępujący fragment. Jeśli chcemy zmienić wszystkie wystąpienia danego słowa lub frazy, musimy użyć odpowiednio `g` lub `gi` na końcu wzorca wyszukiwania. Oznacza to globalne wyszukiwanie (dla wszystkich wystąpień) oraz ignorowanie wielkości liter.

## Głębsza analiza

Warto także wiedzieć, że metoda `replace()` zwraca nowy tekst, a nie zmienia oryginał. Jeśli chcemy zmienić oryginalny tekst, musimy przypisać nowy tekst do zmiennej, jak pokazano w przykładzie.

Ponadto, metoda `replace()` może także przyjmować wyrażenia regularne jako parametry, co pozwala na bardziej rozbudowane i precyzyjne wyszukiwanie i zamianę tekstu. Jest to jednak tematem bardziej zaawansowanym, więc zachęcam do zgłębienia go na własną rękę.

## Zobacz także

- [Dokumentacja metody `replace()` w MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/replace)
- [Dokumentacja metody `replaceAll()` w MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/replaceAll)
- [Wyrażenia regularne w JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)

Dziękuję za przeczytanie tego wpisu i mam nadzieję, że teraz czujesz się pewniej w zakresie wyszukiwania i zamiany tekstu w JavaScript!