---
title:    "Fish Shell: Pobieranie podciągów"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego życia. Wiele z nas codziennie korzysta z różnych narzędzi wykorzystujących języki programowania. Jedną z popularnych opcji jest Fish Shell, które pozwala nam na automatyzację wielu zadań. Jedną z przydatnych funkcji w Fish Shell jest możliwość wyciągania podciągów z tekstu. W tym artykule dowiesz się, dlaczego warto poznać tę funkcję oraz jak jej używać.

## Jak to zrobić

Aby wyciągać podciągi w Fish Shell, używamy polecenia `string` oraz `cut`. Przykładowo, jeśli chcemy wyciągnąć pięć pierwszych znaków z tekstu "Programowanie jest fajne", używamy poniższego kodu:

```Fish Shell
set tekst "Programowanie jest fajne"
string sub -l 5 $tekst
```

Wynik wyświetli się w terminalu jako "Progr".

Możemy również wyciągać podciągi używając indeksów. Przykładowo, jeśli chcemy wyciągnąć pierwsze trzy znaki z tekstu, używamy poniższego kodu:

```Fish Shell
set tekst "Programowanie jest fajne"
cut -c 1-3 $tekst
```

Wynik wyświetli się jako "Pro".

Możliwości wyciągania podciągów są nieograniczone. Możemy używać różnych funkcji, takich jak `substr` czy `find`, aby dopasować nasze potrzeby.

## Głębsze zagłębienie

Wyciąganie podciągów jest przydatne w różnych sytuacjach. Możemy wykorzystywać je do filtrowania danych, dzielenia tekstu na mniejsze części, czy też uzyskania tylko interesujących nas informacji. Jest to ważna umiejętność, którą warto poznać w celu ułatwienia sobie pracy z codziennymi zadaniami.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o funkcjach Fish Shell, polecamy zapoznanie się z poniższymi artykułami:

- [Fish Shell – szybka i przyjazna powłoka dla programistów](https://geekblog.pl/post/fish-shell-szybka-i-przyjazna-powloka-dla-programistow/)
- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [5 przydatnych funkcji Fish Shell, które musisz poznać](https://www.nixtutor.com/linux/5-useful-fish-shell-features-you-should-know/)

Sprawdź również naszą stronę, gdzie znajdziesz wiele ciekawych artykułów na temat programowania.