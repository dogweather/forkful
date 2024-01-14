---
title:    "Clojure: **Pisanie do standardowego wyjścia błędu"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Dlaczego pisać do standardowego błędu?

Pisanie do standardowego błędu jest bardzo pomocne podczas pisania programów w Clojure. Umożliwia wyświetlanie błędów i komunikatów o wyjątkach, dzięki czemu łatwiej jest nam debugować kod i naprawiać ewentualne problemy.

## Jak to zrobić?

Aby pisać do standardowego błędu, możemy użyć funkcji `println` w połączeniu z `System/err`. Przykładowy kod wyglądałby następująco:

```Clojure
(println "Błąd: To jest wiadomość błędu" (System/err))
```

W powyższym przykładzie użyliśmy `println` do wyświetlenia tekstu "Błąd: To jest wiadomość błędu". Następnie, w drugim argumencie tej funkcji, przekazaliśmy `System/err`, co spowodowało, że wiadomość została wysłana na standardowy błąd.

## Głębszy zanurzenie

Ważne jest, aby pamiętać, że standardowy błąd jest konsolą, która jest wykorzystywana do wyświetlania informacji o błędach i wyjątkach. Dlatego nie powinno się nadużywać tej funkcjonalności i wyświetlać na standardowym błędzie wszystkich komunikatów i informacji. Powinniśmy skupić się na wykorzystaniu go tylko w przypadkach, gdy rzeczywiście wystąpił błąd lub problem.

## Zobacz również

- [Dokumentacja funkcji println w Clojure](https://clojuredocs.org/clojure.core/println)
- [Tutorial o standardowym wyjściu i błędzie w Clojure](https://www.clojureinaction.com/writing-to-standard-output-and-error/)