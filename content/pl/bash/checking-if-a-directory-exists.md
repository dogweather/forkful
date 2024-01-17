---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Bash: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Sprawdzanie czy istnieje katalog jest procesem, w którym programista sprawdza czy dany katalog istnieje w systemie plików. Jest to ważne, ponieważ pozwala uniknąć błędów w skryptach, które mogą być spowodowane nieistniejącym katalogiem.

## Jak to zrobić:
```Bash
if [ -d <nazwa_katalogu> ]
then
    echo "Katalog istnieje."
else
    echo "Katalog nie istnieje."
fi
```
**Opis**:
W powyższym przykładzie wykorzystywana jest opcja ```-d``` w warunku ```[ ... ]```, która sprawdza czy w systemie plików istnieje dany katalog. Jeśli tak, to wykonywane jest polecenie zamieszczone po słowie kluczowym ```then```, w tym przypadku wyświetlenie informacji o istnieniu katalogu. W przeciwnym wypadku, czyli gdy katalog nie istnieje, wyświetlana jest informacja o jego braku.
Warto zauważyć, że w powyższym kodzie należy podać pełną ścieżkę do katalogu, np. ```/home/użytkownik/katalog```.

## Pogłębione zagadnienia:
**Historia**: Początkowo, w systemie Unix, nie istniała możliwość sprawdzenia czy katalog istnieje. Dopiero wraz z rozwojem powłoki bash (Bourne Again Shell), pojawiła się opcja ```-d```, dzięki której możemy to łatwo sprawdzić.

**Alternatywy**: Istnieje kilka sposobów, aby sprawdzić czy katalog istnieje. Można wykorzystać różne warunki, takie jak ```-e``` (czy plik / katalog istnieje), ```-f``` (czy jest to plik regularny) lub ```-r``` (czy plik / katalog jest dostępny do odczytu). Jednak opcja ```-d``` jest najbardziej odpowiednia, gdy chcemy sprawdzić czy istnieje właśnie katalog.

**Szczegóły implementacji**: Dzisiejsze systemy uniksowe (w tym macOS) wykorzystują jądro Linux, które oferuje dużo bardziej efektywny sposób sprawdzania czy katalog istnieje. Wcześniej używane funkcje, takie jak `stat()` lub `access()`, były mniej wydajne i wymagały więcej kodu, aby uzyskać ten sam efekt.

## Zobacz również:
- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Poradnik Linuxa](https://www.linuxtutorial.org/how-to-check-if-a-directory-exists-in-a-shell-script/)