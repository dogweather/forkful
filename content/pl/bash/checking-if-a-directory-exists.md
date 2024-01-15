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

## Dlaczego

Sprawdzanie czy dany katalog istnieje jest ważnym aspektem w programowaniu i skrypcie Bash. Wiele zadań wymaga dostępu do określonych katalogów, a niektóre operacje mogą nawet wywołać błędy, jeśli dany katalog nie istnieje. Dlatego ważne jest, aby wykonywać sprawdzenie istnienia katalogu przed wykonaniem dalszych poleceń.

## Jak to zrobić

Aby sprawdzić czy katalog istnieje w skrypcie Bash, użyjemy polecenia `test` w połączeniu z flagą `-d`, która oznacza "czy istnieje katalog". Przykładowy kod wyglądałby następująco:

```Bash
if test -d moj_katalog; then
  echo "Katalog istnieje!"
else
  echo "Katalog nie istnieje!"
fi
```

Powyższy kod sprawdzi, czy katalog o nazwie "moj_katalog" istnieje w bieżącym miejscu, w którym znajduje się nasz skrypt. Jeśli tak, wyświetli odpowiedni komunikat, jeśli nie - wyświetli inny.

Możemy także użyć polecenia `test` w połączeniu z flagą `-e`, która sprawdza istnienie dowolnego pliku lub katalogu. Na przykład:

```Bash
if test -e /usr/bin/bash; then
  echo "Bash jest zainstalowany!"
else
  echo "Bash nie jest zainstalowany!"
fi
```

W naszym przykładzie sprawdzamy, czy plik wykonywalny Bash znajduje się w katalogu /usr/bin. Jeśli tak, wyświetlimy odpowiedni komunikat, jeśli nie - wyświetlimy inny.

## Dogłębna analiza

W rzeczywistości, polecenie `test` jest tylko aliasem dla innego polecenia - `[`. Wywołanie `test -d moj_katalog` jest równoznaczne z wywołaniem `[ -d moj_katalog ]`. `test` jest poleceniem wbudowanym w Bash, ale `[` jest zewnętrznym programem znajdującym się w katalogu /bin.

`test` lub `[` przyjmuje wiele flag i opcji, które pozwalają na różne rodzaje testów, nie tylko sprawdzanie istnienia plików i katalogów. Możemy na przykład sprawdzić czy plik jest pusty, czy czytelnik lub wykonywalny, czy jego rozmiar jest większy/mniejszy od określonej wartości, itp. Aby poznać pełną listę wszystkich opcji `test`, wpisz w terminalu `man test`.

## Zobacz również

- [Bash Beginners Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/chap_07.html)
- [Sekwencje ucieczki w skrypcie Bash](https://linux.die.net/abs-guide/escape-characters.html)
- [Podstawowe instrukcje warunkowe w skrypcie Bash](https://linux.die.net/abs-guide/testconstructs.html)