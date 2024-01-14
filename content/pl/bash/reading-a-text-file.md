---
title:                "Bash: Odczytywanie pliku tekstowego"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest jedną z najważniejszych umiejętności, zwłaszcza dla osób związanych z technologią. Bash, czyli popularny język skryptowy w systemach Linux i Unix, jest często wykorzystywany do automatyzacji wielu zadań. Jego podstawowa funkcjonalność obejmuje m.in. czytanie i edycję plików tekstowych. W tym artykule dowiesz się, dlaczego warto nauczyć się czytać pliki tekstowe w Bashu.

## Jak To Zrobić

Aby czytać pliki tekstowe w Bashu, możesz wykorzystać kilka prostych komend, takich jak `cat`, `grep` czy `awk`. Przykładowo, jeśli chcesz wyświetlić zawartość pliku tekstowego, możesz użyć komendy `cat nazwa_pliku`. Jeśli chcesz znaleźć konkretną frazę w pliku, użyj komendy `grep "fraza" nazwa_pliku`. Natomiast komenda `awk` pozwala wykonywać bardziej zaawansowane operacje na plikach tekstowych, na przykład wyświetlanie konkretnych kolumn lub sortowanie danych.

Poniższy przykład kodu przedstawia użycie komendy `awk` do wyświetlania pierwszej kolumny z pliku `dane.csv`:

```Bash
awk -F"," '{print $1}' dane.csv
```

Wynik wykonania tej komendy może wyglądać następująco:

```
Imię
Adam
Ewa
Jan
```

## Deep Dive

Czytanie plików tekstowych w Bashu może wydawać się proste i przydatne, ale istnieje wiele bardziej zaawansowanych funkcjonalności, które warto poznać. Możesz na przykład wykorzystać pętlę `while` lub `for` do czytania plików linia po linii i wykonywania na nich różnych operacji. Możliwości są praktycznie nieograniczone, a im bardziej poznasz Bash, tym więcej pomysłów będziesz miał na wykorzystanie czytania plików tekstowych.

## Zobacz również

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/)
- [10 przykładów czytania plików tekstowych w Bashu](https://www.cyberciti.biz/faq/unix-howto-read-line-by-line-from-file/)
- [Inne przydatne komendy Bash](https://www.codecademy.com/articles/touch-and-mkdir)

Dziękujemy za przeczytanie tego artykułu na temat czytania plików tekstowych w Bashu. Mamy nadzieję, że przydał się on w Twojej przygodzie z programowaniem. Powodzenia!