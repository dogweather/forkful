---
title:    "Fish Shell: Zmiana tekstu na duże litery"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Zapewne zdarzyło Ci się kiedyś napisać imię lub tytuł w małych literach i zorientować się, że powinien być napisany z wielkiej litery. Ręczne zmienianie dużych i małych liter może być uciążliwe i czasochłonne, zwłaszcza jeśli masz do czynienia z długim tekstem. W takich sytuacjach, warto poznać prostą funkcję w Fish Shell, która może Ci pomóc w automatycznym kapitalizowaniu tekstu.

## Jak to zrobić

Aby kapitalizować string w Fish Shell, wystarczy użyć polecenia `string capitalize`. Możesz podać string, który chcesz zmienić jako argument polecenia:

```Fish Shell
string capitalize "witaj świecie"
```

W wyniku otrzymasz "Witaj Świecie". Możesz również podać więcej niż jeden string jako argument, jeśli chcesz zmienić kilka wyrazów jednocześnie. Ponadto, jeśli chcesz wykorzystać kapitalizowanie do zmiany nazw plików lub folderów, możesz użyć `string capitalize` w połączeniu z pętlą `for`:

```Fish Shell
for file in *.txt
	string capitalize $file
end
```

Powoduje to zapisanie każdego pliku z rozszerzeniem txt z dużą literą na początku, na przykład "Raport.txt" zamiast "raport.txt".

## Deep Dive

Ważne jest, aby zauważyć, że funkcja `string capitalize` zachowuje białe znaki w tekście. Dzięki temu, jeśli chcesz zachować oryginalny układ tekstu, możesz po prostu użyć `string capitalize` i zachować pierwotny wygląd. Poniższy przykład pokaże tę zależność:

```Fish Shell
string capitalize "obliczenia w bazie danych"
```

W wyniku otrzymamy "Obliczenia W Bazy Danych". Białe znaki pozostają w swoich miejscach, a jedynym zmienionym elementem są wielkie litery.

Jeśli chcesz kapitalizować tylko pierwszą literę tekstu, możesz użyć polecenia `string capitalize -s`. Spowoduje to, że tylko pierwsza litera w całym tekście będzie z dużą literą.

## Zobacz też

- Dokumentacja Fish Shell na temat funkcji kapitalizacji tekstu: https://fishshell.com/docs/current/cmds/string.html#capitalize
- Inne przydatne funkcje w Fish Shell: https://fishshell.com/docs/current/cmds.html