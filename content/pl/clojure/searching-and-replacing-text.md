---
title:    "Clojure: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas pisania kodu, potrzebujemy szybkiego sposobu na zmianę tekstu w naszym kodzie. Może to być konieczne, jeśli zmieniliśmy nazwę zmiennej lub funkcji, lub chcemy zmienić pewne słowa na inne. Szukanie i zastępowanie tekstu może znacznie przyspieszyć ten proces, a w Clojure jest to proste do wykonania.

## Jak to zrobić?

W Clojure możemy użyć funkcji `clojure.string/replace` do wyszukania i zastąpienia tekstu. Podajemy tej funkcji trzy argumenty: string, który chcemy zmodyfikować, wyrażenie regularne (regular expression), które oznacza tekst, który chcemy wyszukać, oraz string, którym chcemy go zastąpić.

Niech naszym przykładem będzie zmiana nazwy zmiennej `liczba` na `numer` w poniższym kodzie:

```Clojure
(def liczba 10)
```

Użyjemy funkcji `clojure.string/replace`, aby zamienić `liczba` na `numer`:

```Clojure
(ns replace-example.core
  (:require [clojure.string :as str]))

(def liczba 10)
(str/replace "liczba" #"liczba" "numer")
=> "numer"
```

Jak widać, funkcja `clojure.string/replace` zwraca nowy string z zastąpionym tekstem.

## Głębsze zanurzenie

W powyższym przykładzie użyliśmy wyrażenia regularnego `"liczba"` jako tekstu, który chcemy zastąpić. Jednakże, wyrażenia regularne dają nam możliwość wykonywania bardziej zaawansowanych operacji wyszukania i zastępowania tekstu.

Na przykład, jeśli chcemy zastąpić wszelkie wystąpienia liczby w naszym stringu na `numer`, możemy użyć wyrażenia regularnego `"[0-9]+"`. Fragment `"[0-9]+"` oznacza, że chcemy dopasować wszystkie cyfry w tekście, a `+` oznacza, że dopasowanie może wystąpić wielokrotnie.

Zastosowanie tego wyrażenia regularnego wyglądać będzie następująco:

```Clojure
(def liczba 10)
(str/replace "liczba 10" #"[0-9]+" "numer")
=> "liczba numer"
```

Widzimy tu, że wszystkie wystąpienia `10` zostały zastąpione przez `numer`.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych lub funkcji `clojure.string/replace`, możesz zapoznać się z poniższymi linkami:

- [Dokumentacja wyrażeń regularnych](https://regexr.com/)
- [Dokumentacja funkcji clojure.string/replace](https://clojuredocs.org/clojure.string/replace)