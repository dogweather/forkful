---
title:                "Fish Shell: Odczytywanie pliku tekstowego."
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub po prostu lubisz eksperymentować z nowymi narzędziami, ten post jest dla Ciebie! Dowiecie się jak wykorzystać powłokę Fish Shell do odczytywania plików tekstowych, co może przydać się w różnych projektach.

## Jak To Zrobić

Pierwszym krokiem jest otworzenie aplikacji Terminal i uruchomienie Fish Shell. Aby odczytać plik tekstowy, musisz użyć wbudowanego polecenia `cat`. Na przykład, jeśli chcemy wyświetlić zawartość pliku o nazwie "tekst.txt", wykonujemy następującą komendę:

```Fish Shell
cat tekst.txt
```

Jeśli plik tekstowy jest w innym folderze, musisz podać jego dokładną ścieżkę. W przypadku, gdy chcesz wyświetlić więcej niż jeden plik, możesz podać ich nazwy rozdzielone spacjami. 

Aby zapisać zawartość pliku do nowego pliku, używamy operatora `>` wraz z nazwą docelowego pliku. Na przykład:

```Fish Shell
cat tekst.txt > nowy_plik.txt
```

To samo możemy osiągnąć poprzez użycie polecenia `tee`, które wyświetli zawartość pliku oraz zapisze ją do nowego pliku. Na przykład:

```Fish Shell
cat tekst.txt | tee nowy_plik.txt
```

## Deep Dive

Podczas odczytywania plików tekstowych z pomocą powłoki Fish Shell, mamy również dostęp do różnych flag i opcji. Na przykład, możemy użyć flagi `-n` aby wyświetlić numerowane linie pliku lub `-5` aby wyświetlić tylko pierwsze pięć linii. Możemy także użyć opcji `-s` aby wyciąć dane z pliku według podanych kryteriów. 

Możemy również wykorzystać polecenie `head` do wyświetlenia pierwszych kilku linii pliku lub `tail` do wyświetlenia jego ostatnich linii. Innym przydatnym narzędziem jest polecenie `less`, które pozwala przewijać plik w pionie oraz zapewnia dodatkowe funkcje, takie jak wyszukiwanie w tekście.

## Zobacz również

- [Oficjalna dokumentacja Fish Shell] (https://fishshell.com/docs/current/)

- [Poradnik Fish Shell dla początkujących] (https://fishshell.com/docs/current/tutorial.html)

- [Materiały edukacyjne i przykładowe kody Fish Shell] (https://fishshell.com/docs/current/index.html)