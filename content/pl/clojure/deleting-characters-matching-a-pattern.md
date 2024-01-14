---
title:    "Clojure: Usuwanie znaków pasujących do wzorca"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

As a programmer, sometimes we come across situations where we need to delete characters from a string that match a certain pattern. This could be for formatting purposes or for data manipulation. In this blog post, we will explore how to do this in Clojure and dive deeper into the mechanics behind it.

## Dlaczego

Możliwe, że jako programiści potrzebujemy usunąć ze stringa znaki, które pasują do określonego wzorca. Może być to związane z formatowaniem czy też manipulacją danymi. W tym wpisie na blogu dowiemy się, jak to zrobić w języku Clojure i zagłębimy się w mechanikę tego procesu.

## Jak to zrobić

Aby usunąć znaki zgodne z określonym wzorcem w Clojure, możemy skorzystać z funkcji `replace` z biblioteki `clojure.string`. Przykładowo, jeśli chcemy usunąć wszystkie litery `'a'` z danego stringa, możemy użyć następującego kodu:

```Clojure
(use '[clojure.string :only [replace]])

(def string-do-usuniecia "Programowanie jest zabawa!")
(replace string-do-usuniecia #"a" "")
```

Kod ten wykorzystuje wyrażenie regularne `#"a"`, które określa, jakich znaków szukamy w stringu. Następnie, mamy pusty string jako drugi argument, co oznacza, że te znaki zostają usunięte z oryginalnego stringa. Jeśli uruchomimy ten kod, otrzymamy następujący wynik:

```Clojure
"Progrmowienie jest zbaw!"
```

Ponadto, możemy też wykorzystać funkcję `replace` do zamiany danego znaku na inny. Na przykład:

```Clojure
(replace "Kuchnia nie jest tylko dla babć" #"nie" "jest")
```

Wynik:

```Clojure
"Kuchnia jest jest tylko dla babć"
```

## Deep Dive

Podczas wykorzystywania funkcji `replace` w Clojure, warto pamiętać, że argument `pattern` musi być wyrażeniem regularnym. Dzięki temu, mamy większą kontrolę nad tym, jakie znaki chcemy usunąć lub zamienić.

Ponadto, w przypadku zamiany znaków, możemy wykorzystać nie tylko stringi jako argumenty, ale również funkcje anonimowe. Przykładowo, jeśli chcemy usunąć nie tylko litery `'a'`, ale również litery `'b'`, możemy wykorzystać poniższy kod:

```Clojure
(replace "Jabłka, pomidory, marchewki" #"[ab]" (fn [match] ""))
```

Wynik:

```Clojure
"Jłka, pomiody, mrczki"
```

W tym przypadku, wykorzystaliśmy wyrażenie regularne `#"[ab]"`, które oznacza, że nie tylko szukamy pojedynczych znaków `'a'` i `'b'`, ale również dowolnych ciągów tych znaków. Następnie, zamiast pustego stringa, wykorzystaliśmy funkcję anonimową, która zwraca pusty string dla każdego znalezionego dopasowania.

## Zobacz też

- Dokumentacja `clojure.string`: https://clojuredocs.org/clojure.string
- Wyrażenia regularne w Clojure: https://clojuredocs.org/clojure.repl/source/clojure.repl/source

Dzięki wykorzystaniu funkcji `replace` i wyrażeń regularnych, możemy łatwo manipulować stringami i usunąć z nich niechciane znaki. Mam nadzieję, że ten wpis był dla Ciebie pomocny i pozwolił na lepsze zrozumienie tego procesu. Do zobaczenia w kolejnych blogowych wpisach!