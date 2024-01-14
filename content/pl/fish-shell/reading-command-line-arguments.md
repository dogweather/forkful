---
title:    "Fish Shell: Odczytywanie argumentów wiersza poleceń"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w wierszu poleceń może wydawać się na pierwszy rzut oka nieprzyjazne i trudne do zrozumienia, jednak jest to bardzo przydatne narzędzie dla programistów. Pozwala na szybkie i precyzyjne wykonywanie poleceń oraz automatyzację zadań. Czytanie argumentów w wierszu poleceń jest jedną z umiejętności, które znacznie ułatwiają pracę z systemem operacyjnym. W tym artykule pokażemy Ci, jak to zrobić przy użyciu powłoki Fish Shell.

## Jak to zrobić

Aby odczytać argumenty z wiersza poleceń, musimy skorzystać z wbudowanego polecenia `set` w Fish Shell. Najczęściej wykorzystywany jest wraz z opcją `-q`, która sprawdza czy dany argument jest obecny w liście argumentów.

```Fish Shell
# Przykład 1
set -q arg1; and echo $status
# Jeśli argument "arg1" jest obecny, wyświetli się wartość "0", w przeciwnym razie "1"

# Przykład 2
if set -q arg1
    echo "Argument arg1 jest obecny w wierszu poleceń"
else
    echo "Nie znaleziono argumentu arg1"
end
# W zależności od wyniku, wyświetli jedną z dwóch wiadomości
```

Powyższe przykłady ilustrują proste sposoby na sprawdzenie obecności argumentów w wierszu poleceń. Jednak czasami potrzebujemy nieco bardziej rozbudowanych funkcjonalności. W takim przypadku możemy zastosować strukturę `switch`, która pozwala na sprawdzanie wielu argumentów jednocześnie.

```Fish Shell
# Przykład 3
switch $argv
case "-h" or "--help"
    echo "Wyświetlam pomoc"
case "-v" or "--version"
    echo "Wyświetlam wersję"
case "-f" or "--file"
    echo "Wyświetlam zawartość pliku $argv[2]"
end
# W zależności od wybranej opcji, wyświetlamy odpowiednią treść
# Przykład wywołania: ./program -f plik.txt
# Wynik: Wyświetlam zawartość pliku plik.txt
```

## Deep Dive

Poza podstawowymi funkcjami do odczytywania argumentów, Fish Shell oferuje także wiele dodatkowych opcji, które warto poznać. Możemy na przykład odczytywać argumenty w innej kolejności, używać wzorców do sprawdzania kolejności argumentów, a także wykorzystywać zmienne specjalne, takie jak `$argv0` do odczytywania nazwy wywoływanego pliku. Pełna dokumentacja dotycząca czytania argumentów w Fish Shell znajduje się na oficjalnej stronie dokumentacji.

## Zobacz także

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/cmds/set.html
- Przydatne porady dla początkujących programistów w wierszu poleceń: https://dev.to/aspittel/5-command-line-tricks-for-beginners-5a6

Dzięki nauce odczytywania argumentów w wierszu poleceń w Fish Shell, będziesz mógł/a wydajniej pracować z systemem operacyjnym i automatyzować wiele zadań. Warto poświęcić trochę czasu na poznanie tych funkcjonalności, ponieważ mogą one znacznie ułatwić Ci pracę.