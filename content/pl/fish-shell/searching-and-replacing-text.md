---
title:                "Fish Shell: Wyszukiwanie i zastępowanie tekstu"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że przy pracy z tekstem, musimy dokonać pewnych zmian w jego zawartości. Może to być zmiana wybranego słowa na inne, lub nawet całkowite usunięcie zbędnego tekstu. W takich sytuacjach bardzo przydatne jest narzędzie do wyszukiwania i zastępowania tekstu. W tym artykule pokażemy, w jaki sposób można wykorzystać Fish Shell do tego celu.

## Jak to zrobić

Aby zacząć korzystać z funkcjonalności wyszukiwania i zastępowania tekstu w Fish Shell, należy użyć polecenia `sed`. Polecenie to pozwala na przeszukiwanie plików oraz wprowadzanie zmian w wybranych fragmentach tekstu. Poniżej przedstawiamy przykładowy kod, który znajduje wszystkie wystąpienia słowa `hello` w pliku `text.txt` i zamienia je na `world`:

```Fish Shell
sed -i 's/hello/world/g' text.txt
```

Po wykonaniu tego polecenia, w plikui `text.txt` wszystkie instancje słowa `hello` zostaną zastąpione przez `world`.

Możemy również użyć polecenia `sed` do zmiany tylko wybranych części tekstu. Przykładowo, chcemy zamienić wszystkie litery `a` na `b` tylko w linijce, która zawiera słowo `example`. W tym celu możemy wykorzystać polecenie `sed` w następujący sposób:

```Fish Shell
sed -i '/example/s/a/b/g' text.txt
```

Dzięki temu, wszystkie litery `a` w linijce z wyrażeniem `example` zostaną zamienione na `b`, a pozostałe części pliku pozostaną niezmienione.

## Głębsza analiza

Polecenie `sed` oferuje wiele możliwości dzięki wykorzystaniu wyrażeń regularnych. Możemy na przykład wykorzystać flagę `g` w poleceniu `sed` aby zamienić wszystkie wystąpienia wybranego słowa lub znaku w całym pliku, a nie tylko jedno. Możemy także wykorzystać flagę `i` aby ignorować wielkość liter. Dzięki temu, polecenie będzie wyszukiwać i zastępować tekst niezależnie od tego, czy jest napisany z małych czy dużych liter.

Polecenie `sed` jest bardzo przydatnym narzędziem w Fish Shell, które pozwala na szybkie i proste wyszukiwanie i zastępowanie tekstu. Warto zapoznać się z bogatymi możliwościami tego polecenia, aby ułatwić sobie pracę z tekstem.

## Zobacz również

1. Oficjalna dokumentacja Fish Shell: https://fishshell.com/docs/current/
2. Dokumentacja polecenia `sed`: https://www.gnu.org/software/sed/manual/sed.html
3. Artykuł o wyrażeniach regularnych: https://www.regular-expressions.info/