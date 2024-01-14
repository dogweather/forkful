---
title:                "Fish Shell: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Dlaczego warto sprawdzać czy istnieje katalog?

Sprawdzanie istnienia katalogu jest ważną umiejętnością w programowaniu w języku Fish Shell. Dzięki temu można uniknąć błędów i zapewnić, że twój skrypt będzie działał poprawnie. W tym artykule dowiesz się dlaczego warto sprawdzać czy katalog istnieje oraz jak to zrobić.

## Jak to zrobić?

Pierwszym krokiem jest użycie polecenia `test` w połączeniu z flagą `-e`, która umożliwia sprawdzenie czy dany plik istnieje. W przypadku katalogów wykorzystujemy kropkę, aby oznaczyć aktualny katalog. Poniższy przykład używa polecenia `if` aby wyświetlić odpowiedni komunikat, jeśli katalog istnieje lub nie istnieje.

```Fish Shell
if test -e .pomoc
    echo "Katalog .pomoc istnieje"
else
    echo "Katalog .pomoc nie istnieje"
end
```
**Przykładowy wynik:**
```
Katalog .pomoc istnieje
```

## Głębsze zagadnienia

W języku Fish Shell możemy także wykorzystać operator logiczny `&&` aby wykonywać inne polecenia w zależności od rezultatu sprawdzenia istnienia katalogu. W poniższym przykładzie wyświetlimy zawartość katalogu `.dane` tylko wtedy, gdy istnieje.

```Fish Shell
test -e .dane && ls .dane
```
**Przykładowy wynik:**
```
plik1.txt
plik2.txt
```

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o programowaniu w języku Fish Shell, polecamy zapoznać się z poniższymi zasobami:

- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Wprowadzenie do Fish Shell na LinuxPl](https://linux.pl/artykuly-wprowadzenie-do-fish-shell)
- [Fish Shell dla początkujących na DevStyleR](https://devstyler.io/pl/fish-shell-dla-poczatkujacych/)

Sprawdzenie istnienia katalogu jest bardzo przydatną umiejętnością, która pomoże Ci w programowaniu w języku Fish Shell. Wykorzystaj te wskazówki w swoich projektach i nie musisz martwić się o błędy związane z nieistniejącymi katalogami.