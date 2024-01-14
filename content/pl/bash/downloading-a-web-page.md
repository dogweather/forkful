---
title:                "Bash: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas przeglądania internetu potrzebujemy dostępu do stron z większą ilością informacji lub chcemy mieć dostęp do nich offline. W takim przypadku, pobranie strony internetowej i zapisanie jej na naszym komputerze może być bardzo przydatne. W tym wpisie pokażę w jaki sposób możemy to zrobić za pomocą programowania w Bash.

## Jak to zrobić

Pobranie strony internetowej w Bash jest bardzo proste dzięki użyciu narzędzia o nazwie `wget`. Wystarczy wywołać polecenie `wget` oraz adres URL strony, którą chcemy pobrać. Na przykład:

```Bash
wget https://www.example.com
```

Możemy także określić nazwę pliku, do którego chcemy zapisać pobraną stronę za pomocą opcji `-O`:

```Bash
wget https://www.example.com -O example.html
```

Po wykonaniu tych poleceń, pobrana strona internetowa zostanie zapisana w pliku `example.html` w bieżącym katalogu.

## Głębsza analiza

Jeśli chcemy pobrać więcej niż tylko pojedynczą stronę, możemy wykorzystać funkcjonalność pętli w Bash. Na przykład, jeśli chcemy pobrać wszystkie stron z serwisu BBC News, możemy wykorzystać pętlę `for` do pobrania stron z numerami 1-10:

```Bash
for ((i=1; i<=10; i++))
do
    wget https://www.bbc.com/news/page/$i
done
```

Kod ten będzie pobierał strony https://www.bbc.com/news/page/1, https://www.bbc.com/news/page/2 itd. aż do 10.

Warto również zauważyć, że `wget` może być również używane do pobierania plików z innych protokołów niż HTTP, takich jak FTP czy nawet BitTorrent.

## Zobacz także

- `wget` dokumentacja: https://www.gnu.org/software/wget/manual/wget.html
- Tutorial o programowaniu w Bash: https://linuxconfig.org/bash-scripting-tutorial-for-beginners
- Przydatne narzędzia dla programistów w Bash: https://blog.newrelic.com/engineering/bash-programming-cheat-sheet/