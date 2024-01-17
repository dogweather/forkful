---
title:                "Używanie wyrażeń regularnych"
html_title:           "PHP: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wykorzystywanie wyrażeń regularnych to wykraczająca poza podstawowe funkcjonalności edytora tekstowego umiejętność, która jest niezbędna dla wielu programistów. Pomaga ona w wyszukiwaniu i manipulowaniu tekstem, co jest szczególnie przydatne przy pracach z dużymi plikami lub bazami danych. Dzięki zastosowaniu wyrażeń regularnych można szybko i skutecznie wykonać wielokrotną i dokładną zmianę tekstu.

## Jak to zrobić:
W PHP wyrażenia regularne są wykorzystywane za pomocą funkcji preg_match i preg_replace. Przykładowy kod poniżej pokazuje, jak można użyć wyrażeń regularnych do znalezienia i zamiany słowa "hello" na "hi":
```PHP
$text = "Hello, world!";
$new_text = preg_replace("/hello/", "hi", $text);

echo $new_text; // Output: Hi, world!
```

## Pogłębiona analiza:
Wyrażenia regularne zostały wynalezione przez amerykańskiego matematyka Stephena Cole Kleene w latach 50. XX wieku i były pierwotnie wykorzystywane w matematyce. Jednakże, szybko zostały zaadaptowane przez programistów i obecnie są popularnym narzędziem w wielu językach programowania.

Alternatywą dla wyrażeń regularnych może być użycie funkcji PHP, takich jak strpos i str_replace, jednakże wyrażenia regularne są bardziej wszechstronne i wygodne w użyciu. W PHP wykorzystywane są silniki wyrażeń regularnych takie jak POSIX, Perl i PCRE, które różnią się nieco składnią i dostępną funkcjonalnością. Dlatego też, ważne jest upewnienie się, że wybieramy odpowiedni silnik do naszych potrzeb.

## Zobacz również:
- Oficjalna dokumentacja PHP dotycząca wyrażeń regularnych: https://www.php.net/manual/en/book.pcre.php
- Narzędzie do testowania i generowania wyrażeń regularnych: https://regex101.com/