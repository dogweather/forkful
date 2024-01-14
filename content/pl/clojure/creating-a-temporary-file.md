---
title:    "Clojure: Tworzenie pliku tymczasowego"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest bardzo przydatne podczas programowania w Clojure. Może to być konieczne do przechowywania danych tymczasowych lub do generowania plików do wykorzystania w innych częściach kodu. W tym artykule dowiesz się, w jaki sposób można stworzyć tymczasowe pliki w prosty i efektywny sposób przy użyciu Clojure.

## Jak to zrobić

W celu stworzenia tymczasowego pliku w Clojure, należy użyć funkcji ```with-open``` wraz z funkcją ```clojure.java.io``` do tworzenia pliku i zapisania w nim zawartości. Przykładowy kod wyglądałby następująco:

```
(with-open [f (clojure.java.io/writer "temp.txt")]
  (.write f "To jest zawartość tymczasowego pliku.")
```

Spójrzmy teraz na wyjaśnienie tego kodu. Najpierw, za pomocą ```with-open```, tworzysz obiekt pliku wraz z zapisem do zmiennej f. Następnie używasz funkcji ```clojure.java.io/writer```, aby określić nazwę tymczasowego pliku (w naszym przypadku "temp.txt"). W końcu, za pomocą ```write```, zapisujesz wybraną zawartość do pliku. Po zakończeniu operacji, plik zostanie automatycznie zamknięty.

## Głębszy zanurzony

Możesz również dodać funkcję ```with-open``` do funkcji ```binding``` w celu utworzenia zmiennej środowiskowej, aby uzyskać dostęp do tymczasowego pliku w innych częściach kodu. Na przykład:

```
(with-open [f (clojure.java.io/writer "temp.txt")]
  (.write f "To jest zawartość tymczasowego pliku.")
  (binding [*out* f]
    (println "Używam zawartości tymczasowego pliku.")))
```

W tym przykładzie, ```with-open``` stworzy tymczasowy plik, a następnie za pomocą ```binding``` utworzysz zmienną ```*out*```, która jest równa obiektowi pliku. W ten sposób można wypisać tekst do tymczasowego pliku, a następnie użyć go w innej części kodu. Pamiętaj jednak, że zmienne środowiskowe są zmienne tylko w ramach bieżącego wywołania.

## Zobacz także

- Dokumentacja Clojure dotycząca funkcji [with-open](https://clojuredocs.org/clojure.core/with-open)
- Opis funkcji [binding](https://clojuredocs.org/clojure.core/binding)
- Przykładowy projekt wykorzystujący tymczasowe pliki [na GitHub](https://github.com/example/temporary-files-example)