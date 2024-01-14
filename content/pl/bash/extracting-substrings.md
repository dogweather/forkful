---
title:    "Bash: Ekstrakcja podciągów"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu w Bash zdarza się, że potrzebujemy wyodrębnić tylko część tekstu z danego stringa. To może być na przykład nazwa pliku lub kawałek tekstu ze zmiennej. W takich sytuacjach przydatne jest wykorzystanie funkcji do wydobywania podciągów.

## Jak to zrobić?

W Bash istnieją różne sposoby na wyciągnięcie podciągu ze stringa, a wszystkie opierają się na wykorzystaniu operatora nawiasów kwadratowych `[]` lub podwójnych nawiasów kwadratowych `[[]]`. Przykładowa składnia wygląda następująco:

```Bash
$ string="To jest przykładowy string"

# Przykład 1: Wyodrębnienie podciągu zaczynając od indeksu 3
$ echo ${string:3}
 jest przykładowy string

# Przykład 2: Wyodrębnienie podciągu zaczynając od indeksu 3 i zawierającego tylko 6 znaków
$ echo ${string:3:6}
 jest p

# Przykład 3: Wyodrębnienie podciągu od końca (należy podać liczby ujemne)
$ echo ${string: -6}
string
```

W powyższych przykładach `string` jest zmienną, a liczby po dwukropku oznaczają konkretny indeks, od którego ma zacząć lub kończyć się wyodrębnianie podciągu.

Istnieje również możliwość użycia operatora `%` i `%%` oraz `#` i `##` do wyodrębnienia podciągów na podstawie wystąpienia danego znaku lub ciągu znaków. Przykłady:

```Bash
$ file="file_name.txt"

# Przykład 1: Wyodrębnienie nazwy pliku bez rozszerzenia
$ echo ${file%.*}
file_name

# Przykład 2: Wyodrębnienie nazwy pliku bez rozszerzenia od końca (należy podać liczbę ujemną)
$ echo ${file%-*}
file
```

## Głębszy wgląd

W przypadku bardziej skomplikowanych operacji na podciągach, można wykorzystać komendę `sed` lub `awk`. Na przykład, aby usunąć wszystkie cyfry z danego ciągu znaków można wykorzystać poniższą komendę z wykorzystaniem `sed`:

```Bash
$ string="The 16 awesome 1 things"

# Wyświetlanie stringa bez cyfr
$ echo $string | sed 's/[0-9]//g'
The  awesome  things
```

Można również wykorzystać `awk` do zaawansowanego przetwarzania podciągów na podstawie warunków i separacji na podstawie danego znaku lub słowa. Przykład:

```Bash
$ string="Hello,World,Code"

# Wyświetlanie pierwszego słowa
$ echo $string | awk -F "," '{print $1}'
Hello
```

## Zobacz także

- [Bash String Manipulation](https://www.baeldung.com/linux/bash-string-manipulation)
- [Useful Bash String Manipulation Techniques](https://linuxize.com/post/bash-string-manipulation/)