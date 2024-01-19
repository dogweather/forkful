---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Clojure: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Czy katalog istnieje w Clojure?

## Co & Dlaczego?

(1) Sprawdzanie, czy katalog istnieje, to prosta operacja, której celem jest uzyskanie informacji, czy określony katalog istnieje w systemie plików. 
(2) Programiści robią to, aby uniknąć błędów podczas próby otwarcia, odczytania lub zapisania do katalogu, który nie istnieje.

## Jak to zrobić:

```Clojure
;; Twój kod tutaj:
;; Zaimportuj klasę File z java.io
(import 'java.io.File)

;; Sprawdź, czy ścieżka do katalogu istnieje i jest faktycznie katalogiem
(defn is-directory? [dir]
  (let [d (File. dir)]
  (and (.exists d) (.isDirectory d))))

;; Przykładowe sprawdzenie
(is-directory? "/home/katalog")
```
Wyjście może być `true` jeśli katalog istnieje lub `false` w przeciwnym razie.

## Deep Dive:

(1) **Kontekst historyczny**: Obiekt `File` w Javie istnieje od pierwszej wersji Java Development Kit (JDK 1.0), wydanej w 1996 roku.

(2) **Alternatywy**: Można również użyć `java.nio.file.Files` i `java.nio.file.Path`, które zostały wprowadzone w Javie 7. Są bardziej uniwersalne i mogą lepiej obsłużyć pliki i katalogi.

```Clojure
;; Kolejny sposób na sprawdzenie, czy katalog istnieje
(defn is-directory-2 [dir]
  (let [d (java.nio.file.Paths/get dir (into-array String []))]
  (java.nio.file.Files/isDirectory d)))
```

(3) **Szczegóły implementacji**: Obie funkcje korzystają z Java interop w Clojure. Metoda `File.exists()` sprawdza, czy plik lub katalog o danej ścieżce istnieje. Metoda `File.isDirectory()` sprawdza, czy istniejący plik to katalog. Łącząc te metody, możemy stwierdzić, czy dana ścieżka jest katalogiem.

## Zobacz również:

1. [Dokumentację Clojure Java Interop](https://clojure.org/reference/java_interop)
2. [Dokumentację Java `File`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
3. [Dokumentację Java `Files`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)
4. [Dokumentację Java `Path`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Path.html)