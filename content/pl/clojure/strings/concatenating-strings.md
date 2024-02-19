---
aliases:
- /pl/clojure/concatenating-strings/
date: 2024-01-20 17:35:08.533320-07:00
description: "\u0141\u0105czenie string\xF3w to po prostu sklejanie ich ko\u0144c\xF3\
  w, tworz\u0105c jeden d\u0142u\u017Cszy tekst. Programi\u015Bci robi\u0105 to, \u017C\
  eby skonstruowa\u0107 wiadomo\u015Bci, dynamiczne URL-e czy\u2026"
lastmod: 2024-02-18 23:08:49.256973
model: gpt-4-1106-preview
summary: "\u0141\u0105czenie string\xF3w to po prostu sklejanie ich ko\u0144c\xF3\
  w, tworz\u0105c jeden d\u0142u\u017Cszy tekst. Programi\u015Bci robi\u0105 to, \u017C\
  eby skonstruowa\u0107 wiadomo\u015Bci, dynamiczne URL-e czy\u2026"
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
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
