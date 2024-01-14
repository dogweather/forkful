---
title:    "Fish Shell: Sprawdzanie czy istnieje katalog"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Dlaczego warto sprawdzić, czy istnieje katalog?

Sprawdzanie istnienia katalogu jest bardzo przydatną umiejętnością w programowaniu, szczególnie w języku Fish Shell. Pozwala to na sprawadzenie, czy dany katalog istnieje i podejmowanie odpowiednich działań w zależności od wyniku. Jest to szczególnie przydatne w skryptach, które wymagają dostępu do określonego katalogu.

## Jak to zrobić?

Aby sprawdzić, czy dany katalog istnieje w Fish Shell, należy użyć polecenia `test -d` w połączeniu z żądanym katalogiem. Na przykład, aby sprawdzić, czy katalog "documents" istnieje w bieżącym katalogu, użyjemy następującego kodu:

```Fish Shell
if test -d documents 
    echo "Katalog documents istnieje"
end
```

W przypadku, gdy katalog nie istnieje, polecenie `echo` nie będzie wykonane.

## Pogłębione zagadnienie

Podstawowe sprawdzenie istnienia katalogu jest bardzo przydatne, ale w niektórych przypadkach może być również potrzebna bardziej szczegółowa informacja o katalogu. W takim przypadku, użyjemy polecenia `stat`, które pozwala na wyświetlenie różnych informacji o pliku lub katalogu, w tym również daty modyfikacji czy rozmiaru.

Przykładowo, poniższy kod wykorzystuje polecenie `stat` do wyświetlenia informacji na temat katalogu "documents":

```Fish Shell
if test -d documents
    stat documents
end
```

Wyjście zawiera wiele informacji i może być wykorzystane do bardziej precyzyjnej analizy katalogu.

# Zobacz również

- Dokumentacja Fish Shell dla polecenia `test`: https://fishshell.com/docs/current/cmds/test.html
- Przewodnik po poleceniu `stat` w Fish Shell: https://fishshell.com/docs/current/cmds/stat.html