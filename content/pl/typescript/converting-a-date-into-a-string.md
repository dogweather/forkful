---
title:                "TypeScript: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na ciąg znaków jest nie tylko kluczowym aspektem programowania TypeScript, ale także ogólnie przydatną umiejętnością. Często musimy wyświetlić daty w czytelnej formie dla użytkowników lub przekazać je jako argument w wywołaniach funkcji. Dzięki konwersji daty na ciąg znaków możemy w prosty sposób osiągnąć ten cel.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w TypeScript, wystarczy użyć dedykowanej metody `toString()` dostępnej na obiekcie `Date`. Przykładowo, jeśli mamy zmienną `date`, która przechowuje datę, za pomocą metody `toString()` możemy przekonwertować ją na ciąg znaków w następujący sposób:

```TypeScript
let date = new Date(2021, 5, 10);
let dateString = date.toString();
```

W takim przypadku, `dateString` będzie zawierać ciąg znaków "Wed Jun 09 2021 00:00:00 GMT+0200 (Central European Summer Time)".

Możemy również użyć specjalnej metody `toLocaleDateString()` do precyzyjnego formatowania daty zgodnie z lokalnymi ustawieniami, np. dla Polski:

```TypeScript
let date = new Date(2021, 5, 10);
let polishDateString = date.toLocaleDateString("pl-PL");
```

W takim przypadku, `polishDateString` będzie zawierać ciąg znaków "09.06.2021". Możemy także podać opcjonalne argumenty, takie jak formatowanie, ilość liczb ułamkowych czy dodatkowe informacje.

## Głębsze zagłębienie

Konwersja daty na ciąg znaków może na początku wydawać się trywialna, jednak warto zwrócić uwagę na kilka aspektów. Po pierwsze, należy uważać na formatowanie daty, które może być różne w zależności od ustawień systemowych lub przeglądarki. Dlatego warto zawsze używać odpowiednich metod, takich jak `toLocaleDateString()`.

Ponadto, warto pamiętać o obsłudze różnych stref czasowych oraz formatów daty, takich jak amerykański MM/DD/YYYY czy angielski DD MMMM YYYY. W takim przypadku, warto wykorzystać biblioteki lub narzędzia, które pomogą nam w precyzyjnej konwersji daty.

## Zobacz także

- [Dokumentacja metod `Date` w TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html)
- [Biblioteka Moment.js do obsługi dat w JavaScript](https://momentjs.com/)
- [Narzędzie date-fns dla bardziej zaawansowanego formatowania daty](https://date-fns.org/)