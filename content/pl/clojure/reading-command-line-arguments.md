---
title:                "Clojure: Odczytywanie argumentów wiersza poleceń"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto zapoznać się ze sposobem czytania argumentów wiersza poleceń? Ponieważ jest to ważna umiejętność w programowaniu, szczególnie przy tworzeniu aplikacji wieloplatformowych. Pozwala to na dostosowanie działania programu do różnych scenariuszy i użytkowników.

## Jak To Zrobić

Aby odczytać argumenty wiersza poleceń w Clojure, używamy funkcji `command-line-opts`. Przykładowy kod wykorzystujący tę funkcję wyglądałby następująco:

```Clojure
(defn print-args []
  (let [args (command-line-opts)]
    (println "Podane argumenty:")
    (doseq [arg args]
      (prn arg))))

(print-args)
```

Przykładowy wynik dla uruchomienia programu z argumentami `--name Liz --age 25` będzie wyglądał następująco:

```
Podane argumenty:
{:name "Liz" :age 25}
```

Funkcja `command-line-opts` zwraca mapę zawierającą wszystkie podane argumenty, gdzie kluczami są nazwy argumentów, a wartościami są ich wartości. Możemy także zdefiniować domyślne wartości dla argumentów poprzez użycie opcji `:default` w funkcji `command-line-opts`.

## Głębszy Przegląd

Funkcja `command-line-opts` jest częścią biblioteki `clojure.tools.cli`, która dostarcza więcej możliwości w zakresie czytania i obsługi argumentów wiersza poleceń. Warto również zapoznać się z opcją `:summary` w funkcji `command-line-opts`, która pozwala na dostosowanie wyświetlanego opisu programu i dostępnych argumentów podczas wywołania programu z opcją `-h`.

## Zobacz Również

- Oficjalna dokumentacja Clojure: https://clojure.org/
- Oficjalne instrukcje do biblioteki clojure.tools.cli: https://github.com/clojure/tools.cli
- Przewodnik po tworzeniu aplikacji wiersza poleceń w Clojure: https://www.braveclojure.com/writing-command-line-applications-in-clojure/