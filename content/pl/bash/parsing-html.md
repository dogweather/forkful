---
title:                "Bash: Przetwarzanie html"
simple_title:         "Przetwarzanie html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Bash i często pracujesz z danymi z internetu, to na pewno wiesz jak ważne jest parsowanie HTML. Bez tego nie byłoby możliwe pobieranie informacji z różnych stron internetowych i przetwarzanie ich w wygodny dla nas sposób.

## Jak to zrobić

Zapewne wiesz już, że do parsowania HTML w Bashu wykorzystuje się narzędzia takie jak Curl, grep czy awk. Możesz także użyć bibliotek takich jak Xmlstarlet lub Jq, aby ułatwić sobie pracę z danymi zaczytanymi z internetu.

Najważniejszą częścią programowania w Bashu jest praktyka. Dzięki niej zrozumiesz lepiej zasady działania narzędzi i będziesz mógł wykorzystać je w swoich projektach. Aby ułatwić ci pierwsze kroki, poniżej przedstawiamy kilka przykładów kodu oraz wyników działania, abyś mógł lepiej zrozumieć jak działa parsowanie HTML w Bashu.

```Bash
# Przykład 1: Pobieranie zawartości strony internetowej i zapisywanie wyniku do pliku

# Za pomocą narzędzia Curl pobieramy zawartość strony z linku http://www.example.com
# Następnie za pomocą grep wybieramy tylko interesującą nas część, a za pomocą > zapisujemy wynik do pliku o nazwie "example.txt"
curl http://www.example.com | grep "Tytuł" > example.txt
```

Wynik:
```Bash
# Zawartość pliku example.txt
Tytuł strony: Example
```

```Bash
# Przykład 2: Wyświetlenie wszystkich linków z danej strony internetowej

# Za pomocą narzędzia Curl pobieramy zawartość strony z linku http://www.example.com
# Następnie za pomocą awk wybieramy tylko interesującą nas część, a za pomocą sort usuwamy powtórzenia
# Na koniec za pomocą uniq wyświetlamy unikalne linki zapisane w jednej linii
curl http://www.example.com | awk -F "href=" '/http/{print $2}' | sort | uniq
```

Wynik:
```Bash
# Wyświetlone linki w jednej linii
"http://www.example.com/pl/article1.html" "http://www.example.com/pl/article2.html" "http://www.example.com/pl/article3.html"
```

## Głębsze zagadnienia

Pomimo tego, że parsowanie HTML w Bashu jest dość proste i przydatne, może być często również wyzwaniem. Nie wszystkie strony internetowe są jednakowo zbudowane i nie zawsze można użyć tych samych narzędzi. Dlatego warto zapoznać się z głębszymi zagadnieniami związanymi z parsowaniem HTML, takimi jak wykorzystanie wyrażeń regularnych, obsługa różnego rodzaju znaczników czy przechwytywanie błędów.

## Zobacz również

- [Dokumentacja Curl](https://curl.haxx.se/docs/)
- [Dokumentacja grep](https://www.gnu.org/software/grep/manual/grep.html)
- [Dokumentacja awk](https://www.tutorialspoint.com/awk/index.htm)
- [Xmlstarlet](http://xmlstar.sourceforge.net/)
- [Jq](https://stedolan.github.io/jq/)