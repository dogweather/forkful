---
title:    "Elm: Znajdowanie długości ciągu znaków"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

Często, podczas programowania w Elm, chcemy sprawdzić długość tekstu, aby na przykład upewnić się, że użytkownik wpisał odpowiednią liczbę znaków w formularzu. W tym artykule dowiesz się, jak w łatwy sposób znaleźć długość ciągu znaków w Elm.

## Jak To Zrobić

Załóżmy, że mamy zmienną o nazwie `tekst`, która zawiera pewien ciąg znaków, na przykład "Witaj w świecie Elm!". Aby znaleźć długość tego tekstu, możemy skorzystać z funkcji `String.length` w Elm. Przykładowe użycie tej funkcji dla zmiennej `tekst` wyglądałoby następująco:

```Elm
tekst = "Witaj w świecie Elm!"
String.length tekst -- zwróci wartość 20
```

Jak widać, wynikiem wywołania funkcji `String.length` jest liczba całkowita, która reprezentuje długość ciągu znaków.

## Głębsze Zagłębienie

Jeśli chcesz lepiej zrozumieć, jak dokładnie działa funkcja `String.length` w Elm, warto przyjrzeć się jej definicji w bibliotece standardowej. Wewnątrz funkcji znajduje się wywołanie funkcji `String.foldl`, która iteruje przez wszystkie znaki w ciągu i zlicza je za pomocą akumulatora. Dzięki temu rozwiązaniu, funkcja `String.length` jest bardzo wydajna i działa nawet dla bardzo długich ciągów znaków.

## Zobacz także
- Dokumentacja funkcji `String.length` w Elm: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Przykładowe użycie funkcji `String.length`: https://elm-lang.org/blog/strings-are-text-not-bytes#length
- Inne przydatne funkcje dla pracy z ciągami znaków w Elm: https://package.elm-lang.org/packages/elm/core/latest/String