---
title:                "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy?

Pisanie testów jest niezbędnym elementem w procesie programowania, ponieważ pozwala nam zweryfikować poprawność naszego kodu i zapobiegać błędom w przyszłości. Odpowiednie testowanie jest kluczowe dla zachowania wysokiej jakości naszych aplikacji.

## Jak pisać testy w Bash?

Pisanie testów w Bash jest prostsze niż się wydaje. Możemy wykorzystać polecenia `test` lub `[[ ]]` do sprawdzania warunków logicznych. Przykładowy kod wyglądałby następująco:

```Bash
#!/bin/bash

string="Hello world"

# Testowanie czy zmienna jest niepusta
test -n "$string"
# Output: 0 (true)

# Testowanie czy zmienna jest pusta
test -z "$string"
# Output: 1 (false)

# Testowanie warunku if-else
if [[ "$string" == "Hello" ]]; then
  echo "Zmienna równa się 'Hello'"
else
  echo "Zmienna nie równa się 'Hello'"
fi
# Output: Zmienna nie równa się 'Hello'
```

W powyższym przykładzie użyliśmy polecenia `test` do sprawdzania, czy zmienna `string` jest pusta lub niepusta. Następnie wykorzystaliśmy warunek `if-else` z użyciem operatora `==` do porównania zmiennej z danym ciągiem znaków.

## Głębszy wgląd w pisanie testów

Tworzenie testów w Bash może być nieco skomplikowane, gdy musimy testować więcej zaawansowane warunki. W takich przypadkach przydatne mogą być polecenia `grep` i `awk`, które umożliwiają przeszukiwanie i analizowanie plików tekstowych. Przykładowy kod wyglądałby następująco:

```Bash
#!/bin/bash

# Pobranie listy plików o rozszerzeniu .txt
files=$(ls *.txt)

# Użycie polecenia grep do przeszukania wszystkich plików zawierających słowo "test"
echo "$files" | grep "test"
# Output: test1.txt, test2.txt

# Użycie polecenia awk do wyświetlenia drugiej kolumny z tekstu (dzieląc go na pola za pomocą "-")
echo "test-1 test-2" | awk -F "-" '{print $2}'
# Output: 2
```

W powyższym przykładzie użyliśmy polecenia `ls` do pobrania listy plików z określonym rozszerzeniem. Następnie, za pomocą `grep` i `awk`, przefiltrowaliśmy i analizowaliśmy dane, aby uzyskać pożądane informacje.

## Zobacz również

- [Bash Guide](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Testing in Bash](http://wiki.bash-hackers.org/commands/classictest)
- [grep Man Page](https://linux.die.net/man/1/grep)
- [awk Man Page](https://linux.die.net/man/1/awk)