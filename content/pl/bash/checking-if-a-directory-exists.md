---
title:                "Bash: Sprawdzanie czy istnieje katalog"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Dlaczego warto sprawdzać, czy istnieje katalog?

Często w programowaniu bashowym konieczne jest sprawdzanie, czy dany katalog istnieje, zanim wykonamy na nim jakieś operacje. Jest to ważne, ponieważ unikamy w ten sposób błędów lub nadpisywania istniejących plików.

# Jak to zrobić?

Do sprawdzenia istnienia katalogu używamy polecenia `test -d [katalog]`, które zwraca wartość `true` lub `false` w zależności od istnienia katalogu. Przykładowy kod wyglądałby następująco:

```Bash
if [ test -d "moj_katalog" ]; then
  echo "Katalog istnieje."
else
  echo "Katalog nie istnieje."
fi
```

W przypadku, gdy chcemy sprawdzić, czy katalog istnieje i jednocześnie jest pusty, możemy użyć polecenia `test -d [katalog] && test -z "$(ls -A [katalog])"`. Oznacza to, że najpierw sprawdzamy istnienie katalogu, a następnie - czy jest on pusty. Jeśli będzie wszystko w porządku, możemy wykonać na nim dowolne operacje.

# Głębszy zanurzenie

Polecenie `test` jest alternatywą dla operatora warunkowego `[ ]` i jest często używane do sprawdzania różnych warunków w skryptach bash. Można użyć go również do sprawdzania istnienia innych elementów, takich jak pliki czy zmienne.

Przykładowo, aby sprawdzić, czy dany plik nie jest pusty, możemy użyć polecenia `test -s [plik]`, które zwróci `true` lub `false` w zależności od tego, czy plik jest niepusty. Możemy również użyć wielu operatorów logicznych, takich jak `&&` czy `||` w celu bardziej złożonych warunków.

# Zobacz również

* [The Linux Command Line: A Complete Introduction](https://www.amazon.com/Linux-Command-Line-Complete-Introduction/dp/1593273894) - książka wprowadzająca w świat programowania w bashu
* [Bash Scripting Tutorial](https://www.shellscript.sh/) - kurs programowania bashowego dla początkujących
* [Bash Reference Manual](https://www.gnu.org/software/bash/manual/html_node/) - oficjalna dokumentacja języka bash