---
title:                "Łączenie łańcuchów znaków"
aliases: - /pl/clojure/concatenating-strings.md
date:                  2024-01-20T17:35:08.533320-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Łączenie stringów to po prostu sklejanie ich końców, tworząc jeden dłuższy tekst. Programiści robią to, żeby skonstruować wiadomości, dynamiczne URL-e czy po prostu połączyć dane w czytelną całość.

## Jak to zrobić:
W Clojure, łączenie stringów jest proste jak śniadanie. Użyj `str`, żeby połączyć kawałki tekstu razem. Oto przykłady:

```clojure
(str "Przykład" " " "prosty")
;=>"Przykład prosty"

;; Łączenie z użyciem zmiennych
(let [first-name "Jan"
      last-name "Kowalski"]
  (str "Cześć, " first-name " " last-name "!"))
;=> "Cześć, Jan Kowalski!"
```
Jeśli masz kolekcję, użyj `clojure.string/join`:
```clojure
(clojure.string/join ", " ["jabłka" "banany" "kiwi"])
;=> "jabłka, banany, kiwi"
```
Jeśli szybkość jest kluczowa, spróbuj użyć `StringBuilder`:
```clojure
(-> (StringBuilder.)
    (.append "Przyśpieszamy ")
    (.append "proces")
    .toString)
;=> "Przyśpieszamy proces"
```

## Deep Dive
Łączenie stringów to prozaiczna, ale kluczowa część programowania od samego początku. W Clojure, `str` jest magią, która wyręcza nas z myślenia o szczegółach. W innych językach, jak Java, bezpośrednie łączenie stringów jest drogie ze względu na niemutowalność stringów — każde `+` to nowy obiekt. Stąd `StringBuilder` w przykładach powyżej — w Javie realizuje ten sam koncept.

Alternatywą jest używanie `format` dla bardziej skomplikowanego formatowania, czy biblioteki jak `clojure.string`, oferująca `join`, która jest idealna, gdy mamy do czynienia z kolekcjami stringów.

Implementacja funkcji `str` jest prosta i efektywna, używa ona Java `StringBuilder` pod spodem. To znaczy, że jest szybka, ale za to nie zawsze idealna przy mega wielkich stringach, gdzie trzeba uważać na używanie pamięci.

## Zobacz również
- Oficjalna dokumentacja `str`: [https://clojuredocs.org/clojure.core/str](https://clojuredocs.org/clojure.core/str)
- Dokumentacja `clojure.string/join`: [https://clojuredocs.org/clojure.string/join](https://clojuredocs.org/clojure.string/join)
