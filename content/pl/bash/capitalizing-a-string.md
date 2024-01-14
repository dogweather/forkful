---
title:    "Bash: Zapisywania wielkich liter w ciągu znaków"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć, programowanie to fascynująca dziedzina, będąca połączeniem sztuki, nauki i logiki. W tym wpisie chciałbym opowiedzieć o jednym z elementów języka Bash - mianowicie o konwersji ciągu znaków na wielkie litery. Możliwe, że niektórzy z Was zastanawiają się po co ta umiejętność i właśnie o tym chcę Was przekonać.

## Jak to zrobić

Aby przekonwertować napis na duże litery w Bashu, możemy użyć wbudowanej funkcji "tr" (od ang. translate). Wywołujemy ją w terminalu w następujący sposób:

```Bash
echo "tekst" | tr a-z A-Z
```

W miejsce słowa "tekst" wstawiamy nasz napis, który chcemy przekonwertować. W wyniku otrzymujemy ten sam napis, ale z wszystkimi literami zamienionymi na duże.

## Głębsza analiza

Powyższy przykład to tylko jedna z metod konwersji napisu na wielkie litery w Bashu. Istnieje także możliwość skorzystania z funkcji "awk", która działa podobnie do "tr". Wykorzystując to narzędzie możemy nawet ustalić precyzyjne parametry konwersji, na przykład zamieniając tylko pierwszą literę w każdym słowie lub pomijając specjalne znaki.

## Zobacz również

Jeśli chcesz poznać więcej o konwersji ciągów znaków na wielkie litery w języku Bash, polecam Ci zapoznać się z poniższymi artykułami:

- [Dokumentacja funkcji "tr" w Bashu](https://www.gnu.org/software/bash/manual/html_node/Program-Output.html)
- [Tutorial na temat manipulowania tekstem w Bashu](https://ryanstutorials.net/linuxtutorial/cheatsheetbash.php)
- [Sekcja poświęcona ciągłom znaków w kursie programowania w Bashu](http://www.tldp.org/LDP/abs/html/string-manipulation.html)