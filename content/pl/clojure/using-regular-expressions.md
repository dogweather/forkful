---
title:    "Clojure: Używanie wyrażeń regularnych"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Języki programowania często wymagają wykorzystania wyrażeń regularnych w celu wykonywania złożonych manipulacji na tekście. Poznajmy, dlaczego warto nauczyć się korzystać z wyrażeń regularnych w Clojure.

## Jak to zrobić

```Clojure
(def text "Witaj w świecie wyrażeń regularnych!")
(matches #"a" text)
```

```markdown
#(0 10)
```

Wyrażenia regularne pozwalają nam na wyszukiwanie i manipulowanie tekstu, wykorzystując określone wzorce. W przykładzie powyżej, użyliśmy wyrażenia regularnego, aby sprawdzić, czy w tekście występuje litera "a". Funkcja `matches` zwraca indeks początkowy i końcowy pierwszego wystąpienia dopasowania. W tym przypadku, litera "a" występuje na indeksach 0 i 10.

```Clojure
(def text "123-456-789")
(re-find #"(\d+)-(\d+)-(\d+)" text)
```

```markdown
#["123-456-789" "123" "456" "789"]
```

Możemy również wykorzystać wyrażenia regularne do wyodrębniania określonych elementów z tekstu. W powyższym przykładzie, po użyciu funkcji `re-find`, otrzymujemy listę z dopasowanymi grupami, czyli liczbami zawartymi wewnątrz nawiasów `(\d+)`.

Jest to tylko wierzchołek góry lodowej w przypadku zastosowań wyrażeń regularnych w Clojure. Istnieje wiele innych funkcji i operatorów, które można wykorzystać, aby wykonywać więcej zaawansowanych operacji na tekście.

## Głębsze zagłębianie się

Podstawowe użycie wyrażeń regularnych zostało przedstawione powyżej, ale warto zapoznać się z bardziej zaawansowanymi operacjami, takimi jak `replace`, `split` czy `re-gsub`. W celu dokładniejszego zrozumienia i wykorzystania wyrażeń regularnych w programowaniu, warto zapoznać się z dokumentacją Clojure oraz różnymi przykładowymi kodami dostępnymi w internecie.

## Zobacz także

- Dokumentacja Clojure: https://clojure.org/api/cheatsheet
- Przykładowy kod wykorzystujący wyrażenia regularne w Clojure: https://gist.github.com/tunahanson/885a3aad7b486419ce46
- Tutorial wyjaśniający wyrażenia regularne w Clojure: https://www.mikedane.com/programming-languages/clojure/regex/