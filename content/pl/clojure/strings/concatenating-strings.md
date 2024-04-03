---
date: 2024-01-20 17:35:08.533320-07:00
description: "Jak to zrobi\u0107: W Clojure, \u0142\u0105czenie string\xF3w jest proste\
  \ jak \u015Bniadanie. U\u017Cyj `str`, \u017Ceby po\u0142\u0105czy\u0107 kawa\u0142\
  ki tekstu razem. Oto przyk\u0142ady."
lastmod: '2024-03-13T22:44:34.986431-06:00'
model: gpt-4-1106-preview
summary: "W Clojure, \u0142\u0105czenie string\xF3w jest proste jak \u015Bniadanie."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

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
