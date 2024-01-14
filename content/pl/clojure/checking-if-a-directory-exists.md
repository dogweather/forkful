---
title:    "Clojure: Sprawdzanie istnienia katalogu"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie czy istnieje katalog jest ważnym elementem tworzenia aplikacji w Clojure. Pozwala to na uniknięcie błędów w przypadku braku katalogu, który jest niezbędny do działania programu. Dzięki takiemu sprawdzeniu, można zapewnić niezawodność i bezpieczeństwo naszej aplikacji.

## Jak to zrobić

### Sprawdzenie istnienia katalogu

W celu sprawdzenia czy dany katalog istnieje, możemy skorzystać z funkcji `fs/exist?` z biblioteki `clojure.java.io`.

```Clojure
(require '[clojure.java.io :as fs])

(fs/exist? "ścieżka/do/katalogu")
```

Jeśli katalog istnieje, funkcja zwróci wartość `true`, w przeciwnym przypadku wartość `false`.

### Obsługa błędów

Aby uniknąć błędów w przypadku nieistniejącego katalogu, możemy skorzystać z funkcji `fs/maybe-file` lub `fs/maybe-dir`, które zwracają `nil` w przypadku braku pliku lub katalogu.

```Clojure
(require '[clojure.java.io :as fs])

(fs/maybe-dir "ścieżka/do/katalogu")
```

W powyższym przykładzie jeśli katalog istnieje, zostanie zwrócony, w przeciwnym przypadku otrzymamy wartość `nil`.

## Deep Dive

Funkcje `fs/exist?`, `fs/maybe-file` i `fs/maybe-dir` wykorzystują podstawową operację systemową, która sprawdza czy dany plik lub katalog istnieje w podanej ścieżce.

Dzięki temu, możemy skorzystać z nich w różnych przypadkach, na przykład w pętlach czy też rekurencyjnych funkcjach, aby sprawdzać istnienie katalogów lub plików na bieżąco i odpowiednio dostosować działanie programu.

## Zobacz również

- Dokumentacja biblioteki `clojure.java.io`: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Poradnik tworzenia aplikacji w Clojure: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)