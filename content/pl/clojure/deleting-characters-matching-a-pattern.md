---
title:                "Clojure: Usuwanie znaków pasujących do wzoru"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usunięcie znaków odpowiadających wzorcowi jest ważnym aspektem programowania w języku Clojure. Pozwala to na efektywne przetwarzanie i manipulację tekstem, co jest niezbędne w wielu projektach. W tym poście wyjaśnimy, dlaczego usuwanie znaków jest przydatne i jak to zrobić.

## Jak to zrobić

```Clojure
(def s "abc-123-def" )
(println (str/replace s #"-" ""))
;; Output: "abc123def"
```

Powyższy przykład pokazuje użycie funkcji ```str/replace```, która dostarcza proste rozwiązanie do usuwania znaków zgodnie z wzorcem. Funkcja ta przyjmuje dwa argumenty: ciąg znaków oraz wyrażenie regularne. W naszym przypadku, wykorzystaliśmy wyrażenie ```#"-"``` aby usunąć wszystkie myślniki z tekstu. Następnie, użycie pustego łańcucha ```""``` jako drugiego argumentu powoduje zastąpienie znalezionych znaków pustym łańcuchem, co w efekcie usuwa je z ciągu. Funkcja ```str/replace``` jest bardzo przydatna w różnych przypadkach usuwania znaków w tekstach.

Inną opcją jest użycie funkcji ```clojure.string/replace```, która również działa w ten sam sposób, ale jest dostępna w standardowej bibliotece Clojure.

Możemy również zastosować zmienne regularne do wybrania konkretnych znaków do usunięcia. Przykład poniżej ilustruje usunięcie wszystkich cyfr z ciągu znaków:

```Clojure
(def s "abc123def" )
(println (str/replace s #"\d" ""))
;; Output: "abcdef"
```
Jest to bardzo przydatne, gdy chcemy pozbyć się określonej grupy znaków, na przykład separatorów lub znaków specjalnych.

## Deep Dive

Jeśli chcesz bardziej zgłębić temat usuwania znaków z tekstu, warto zapoznać się z funkcją ```clojure.string/replace-first```, która usuwa tylko pierwsze wystąpienie zgodne z wyrażeniem regularnym. Możemy również zastosować dodatkowe opcje w nawiasach klamrowych, takie jak zmiana wielkości znaków oraz ograniczenie liczby wystąpień zastępowania.

Inną podstawową funkcją do usuwania znaków jest ```clojure.string/replace-last```, która analogicznie działa jak ```/replace-first```, ale usuwa tylko ostatnie wystąpienie zgodne z wyrażeniem regularnym.

## Zobacz również

- https://clojuredocs.org/clojure.string/replace
- https://clojuredocs.org/clojure.string/replace-first
- https://clojuredocs.org/clojure.string/replace-last