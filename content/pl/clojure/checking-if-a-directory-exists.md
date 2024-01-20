---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Bash: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Sprawdzanie, czy folder istnieje, to proces weryfikacji obecności katalogu w systemie plików. Programiści robią to, aby uniknąć błędów podczas próby dostępu lub modyfikacji nieistniejących ścieżek.

## How to: (Jak to zrobić:)
W Clojure, używamy funkcji `file-seq` wraz z `some` do sprawdzenia, czy katalog istnieje. Oto przykład:

```Clojure
(import '[java.io File])

(defn directory-exists? [path]
  (some #(-> % .isDirectory) (file-seq (File. path))))

(println (directory-exists? "/path/to/directory")) ; true lub false, w zależności od przypadku
```

Jeśli folder istnieje, funkcja zwróci `true`. W przeciwnym razie otrzymasz `false`.

## Deep Dive (Zanurzenie się głębiej)
Checking czy katalog istnieje sięga czasów przed systemami wersji kontroli, gdy programy musiały bezpośrednio zarządzać plikami. W Clojure, mamy kilka sposobów, żeby to zrobić:

1. Jako że Clojure działa na JVM, możesz użyć `java.io.File` lub `java.nio.file.Files` wraz z Java interop.
2. Alternatywnie, są biblioteki takie jak `clojure.java.io`, które zapewniają clojurystyczne API do pracy z plikami i katalogami.

To powiedziawszy, `file-seq` i `some`, są często wystarczające i idiomatyczne dla Clojure. Wydajność natomiast zależy od wielkości drzewa katalogów, które `file-seq` musi przeszukać.

## See Also (Zobacz też)
- Clojure API docs on `file-seq`: [https://clojuredocs.org/clojure.core/file-seq](https://clojuredocs.org/clojure.core/file-seq)
- Java interop guide for Clojure: [https://clojure.org/reference/java_interop](https://clojure.org/reference/java_interop)
- clojure.java.io documentation: [https://clojuredocs.org/clojure.java.io](https://clojuredocs.org/clojure.java.io)