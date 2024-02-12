---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases:
- /de/clojure/checking-if-a-directory-exists/
date:                  2024-02-03T19:07:13.995747-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis in Clojure existiert, umfasst die Verifizierung der Existenz eines Dateisystemverzeichnisses von innerhalb Ihrer Clojure-Anwendung. Diese Aufgabe ist entscheidend für Dateioperationen, um Fehler beim Lesen von oder Schreiben in Verzeichnisse, die möglicherweise nicht vorhanden sind, zu verhindern, was eine robuste und fehlerfreie Codeausführung gewährleistet.

## Wie man es macht:
Da Clojure eine JVM-Sprache ist, kann es für diesen Zweck die `java.io.File` Klasse von Java nutzen. Für eine solch grundlegende Operation benötigen Sie keine Drittanbieter-Bibliothek. So können Sie es machen:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; Beispielverwendung
(println (directory-exists? "/path/to/your/directory")) ;; wahr oder falsch
```

Diese Funktion, `directory-exists?`, nimmt einen Verzeichnispfad als String entgegen und gibt `true` zurück, wenn das Verzeichnis existiert, und `false` anderweitig. Dies wird erreicht, indem ein `File`-Objekt mit dem Verzeichnispfad erstellt und dann die Methode `.exists` auf dieses Objekt aufgerufen wird.

Zusätzlich zur direkten Java-Interop-Kompatibilität können Sie Clojure-Bibliotheken nutzen, die einen Teil des Java-Boilerplates abstrahieren. Eine solche Bibliothek ist `clojure.java.io`. Allerdings würden Sie für das Überprüfen, ob ein Verzeichnis existiert, immer noch die `File`-Klasse verwenden, aber Sie könnten die Bibliothek nützlich für andere Dateioperationen finden. Beispiel:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; Beispielverwendung
(println (directory-exists?-clojure "/another/path/to/check")) ;; wahr oder falsch
```

Diese Version ist recht ähnlich, verwendet jedoch die Clojure `io/file`-Funktion, um das `File`-Objekt zu erstellen. Diese Methode fügt sich natürlicher in Clojure-Codebasen ein, indem sie Clojures Bibliothek für IO-Operationen nutzt, anstatt direkt mit Java-Klassen zu interagieren.
