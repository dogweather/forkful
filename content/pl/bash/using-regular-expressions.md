---
title:                "Używanie wyrażeń regularnych"
html_title:           "Bash: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Regularne wyrażenia są narzędziem wykorzystywanym przez programistów do wyszukiwania i manipulacji tekstem w sposób bardziej skuteczny i efektywny. Pozwalają one na zdefiniowanie wzorców, których szukamy w tekście i wykonaniu na nich określonych operacji.

# Jak to zrobić:

W Bashu możemy użyć polecenia `grep` do wyszukiwania wzorców w plikach tekstowych. Na przykład, jeśli chcemy wyszukać wszystkie linie zaczynające się od słowa "Hello", można to zrobić za pomocą polecenia `grep "^Hello" nazwa_pliku.txt`. Wynik wyświetli się na ekranie.

Możemy również wykorzystać regulaminowe wyrażenia do zastępowania wybranych części tekstu. Na przykład, jeśli chcemy zamienić wszystkie wystąpienia słowa "cat" na "dog" w pliku tekstowym, użyjemy polecenia `sed 's/cat/dog/g' nazwa_pliku.txt`.

# Wnikliwe spojrzenie:

Regularne wyrażenia zostały wprowadzone w 1951 roku przez amerykańskiego matematyka Stephena Kleene. Z czasem zostały one wykorzystane w wielu językach programowania, w tym w Bashu. W Bashu możemy również wykorzystać wyrażenia regularne do sprawdzania poprawności wprowadzanego przez użytkownika tekstu lub do wykonywania bardziej skomplikowanych operacji na stringach.

Alternatywnym podejściem do użycia regulaminowych wyrażeń jest wykorzystanie narzędzi takich jak AWK czy Perl, które posiadają wbudowaną obsługę wyrażeń regularnych. Jednakże, wykorzystanie wyrażeń regularnych w Bashu jest wygodne, ponieważ jest to wbudowane polecenie i nie wymaga dodatkowych instalacji.

# Zobacz również:

- [Bash: Polecenie Grep](https://pl.wikipedia.org/wiki/Grep)
- [Oficjalna Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)