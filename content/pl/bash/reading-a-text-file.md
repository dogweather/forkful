---
title:    "Bash: Odczytywanie pliku tekstowego"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś początkującym programistą lub chcesz po prostu nauczyć się bardziej skomplikowanych zadań w Bash, przeczytanie pliku tekstowego może być bardzo przydatne. W tym krótkim artykule pokażemy Ci, jak czytać pliki tekstowe w Bash i jakie są najczęstsze zastosowania tej umiejętności.

## Jak to zrobić

Czytanie plików tekstowych w Bash jest bardzo proste i wykorzystuje komendę `cat`. Umożliwia ona wyświetlenie zawartości pliku tekstowego w konsoli. W poniższych przykładach użyjemy pliku o nazwie "dane.txt", ale możesz zastąpić ją dowolnym innym plikiem tekstowym.

### Przykład 1: Wyświetlenie całego pliku

Poniższe polecenie wyświetli całą zawartość pliku "dane.txt" w konsoli:

```Bash
cat dane.txt
```

### Przykład 2: Wyświetlenie wybranego wiersza

Możesz również wyświetlić tylko wybrany wiersz z pliku tekstowego, podając jego numer. Na przykład, jeśli chcesz wyświetlić drugi wiersz, użyj poniższego polecenia:

```Bash
cat dane.txt | awk 'NR==2'
```

### Przykład 3: Wyświetlenie wybranej kolumny

Polecenie `awk` może również pomóc w wyświetleniu wybranej kolumny z pliku tekstowego. W poniższym przykładzie wyświetlimy drugą kolumnę z pliku "dane.txt":

```Bash
cat dane.txt | awk '{print $2}'
```

### Przykład 4: Zastosowanie pętli dla każdego wiersza

Jeśli chcesz wykonać jakieś operacje dla każdego wiersza pliku tekstowego, możesz wykorzystać pętlę `while` w Bash. W poniższym przykładzie wyświetlimy każdy wiersz pliku "dane.txt" w osobnej linii:

```Bash
while read line; do
    echo $line
done < dane.txt
```

## Deep Dive

Często będziesz musiał pracować z większą ilością danych w plikach tekstowych i konieczne będzie przetworzenie ich w sposób bardziej złożony. W Bash istnieje wiele narzędzi, które mogą pomóc w takich zadaniach, takich jak `grep` czy `sed`. Niektóre programy wykorzystujące te narzędzia mogą nawet przyjmować plik tekstowy jako argument.

Dla przykładu, poniższe polecenie wyświetli liczbę słów w pliku tekstowym "dane.txt":

```Bash
grep -o '\w\+' dane.txt | wc -w
```

## Zobacz również

- [Podstawowe polecenia Bash](https://linux.die.net/man/1/bash)
- [Polecenie `cat` w Bash](https://linux.die.net/man/1/cat)
- [Polecenie `awk` w Bash](https://linux.die.net/man/1/awk)