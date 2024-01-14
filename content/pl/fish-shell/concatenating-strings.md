---
title:    "Fish Shell: Łączenie ciągów znaków"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Wiele z nas może się zastanawiać, dlaczego w ogóle warto poświęcać czas na łączenie ciągów tekstowych w programowaniu. Jednakże konkatenacja stringów jest niezbędnym elementem wielu aplikacji i pozwala na tworzenie bardziej zaawansowanych, interaktywnych programów. W tym artykule dowiecie się, dlaczego łączenie stringów jest tak ważne w świecie programowania.

## Jak to zrobić

Zanim przejdziemy do wyjaśnienia, dlaczego, najpierw musimy zrozumieć, jak można połączyć stringi w języku Fish Shell. Istnieją różne metody, ale najprostszym sposobem jest użycie operatora "+" lub funkcji string.join. Przykładowy kod wyglądałby tak:

```Fish Shell
set nazwa "Jan"
set nazwisko "Kowalski"

echo $nazwa" "$nazwisko

lub

echo (string join " " $nazwa $nazwisko)
```
Output: Jan Kowalski

W pierwszym przykładzie użyliśmy operatora "+" do połączenia dwóch zmiennych zawierających imię i nazwisko. W drugim przykładzie wykorzystaliśmy funkcję string.join, która przyjmuje jako argument separator oraz listę elementów do połączenia. W obu przypadkach uzyskaliśmy ten sam wynik.

## Deep Dive (Głębsze informacje)

Konkatenacja stringów jest jednym z podstawowych zadań w programowaniu. Polega ona na łączeniu dwóch lub więcej ciągów tekstowych w jeden, dłuższy ciąg. W Fish Shell, stringi są traktowane jako tablice, co oznacza, że można na nich wykonywać operacje takie jak w Pythonie czy Ruby.

Istnieje jednak pewna różnica w tym języku. W Fish Shell, stringi są reprezentowane przez pojedyncze znaki, a nie przez obiekty, co oznacza, że nie można wywoływać na nich metod. W przypadku funkcji, takiej jak string.join, która działa na obiektach typu tablica, musimy najpierw zamienić stringi na tablice za pomocą funkcji string split.

Należy również pamiętać, że konkatenacja ciągów jest wykonywana od lewej do prawej. Jeśli więc chcemy zachować kolejność fragmentów naszego tekstu, musimy uważnie przemyśleć kolejność elementów przed połączeniem.

## Zobacz również

- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/)

- [Tutorial wideo o konkatenacji stringów w Fish Shell](https://www.youtube.com/watch?v=uXfchXoNBgM) 

- [Inne przydatne funkcje Fish Shell](https://opensource.com/article/17/5/fish-shell)