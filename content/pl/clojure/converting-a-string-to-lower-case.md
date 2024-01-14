---
title:    "Clojure: Konwertowanie ciągu znaków na małe litery"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek napisałeś kod, który potrzebuje przekonwertować tekst na małe litery? Może jesteś częścią zespołu, który wprowadza zmiany do już istniejącego kodu i musisz dostosować nazwy zmiennych lub funkcji do ustalonych zasad? W obu przypadkach konwersja stringa na małe litery może okazać się niezbędna. W tym artykule dowiesz się, jak to zrobić w języku Clojure.

## Jak to zrobić

Proces konwertowania stringa na małe litery jest nie tylko prosty, ale również bardzo przydatny. W Clojure możesz wykorzystać funkcję `lower-case` do wykonania tej operacji. Przyjrzyjmy się przykładowemu kodowi:

```
Clojure (lower-case "HELLO WORLD")
```

Powyższy kod zwróci nam wynik: 
```
hello world
```

Możemy również wykorzystać tę funkcję w połączeniu z innymi funkcjami, na przykład `string/split` oraz `string/join`, aby przetworzyć duży tekst i zachować spójność wielkości liter.

```
Clojure (-> "JAKIŚ TEKST Z DUŻYCH LITER" (lower-case) (string/split #" ") (string/join "-"))
```

Output: 
```
jakis-tekst-z-duzych-liter
```

## Głębszy wgląd

Konwertowanie stringa na małe litery może wydawać się prostym zadaniem, ale warto zrozumieć, jak dokładnie działa funkcja `lower-case`. W Clojure jest ona implementowana w oparciu o standardową funkcję dostępną w Java, `toLowerCase()`. Funkcja ta zamienia wszystkie znaki alfabetyczne na ich odpowiedniki z małymi literami, a pozostawia pozostałe znaki bez zmian.

Inną przydatną funkcją jest `string/lower-case`, która pozwala na dokładniejszą kontrolę konwersji. Dostępne są w niej opcje takie jak ignorowanie wielkości liter przy porównywaniu stringów oraz spolszczenie znaków spełniających polski standard tekstu.

## Zobacz także

- Dla dalszej nauki języka Clojure zapoznaj się z dokumentacją oficjalną: https://clojure.org/documentation
- Aby dowiedzieć się więcej o funkcjach dostępnych w Clojure, zajrzyj na stronę ClojureDocs: https://clojuredocs.org/
- Zapoznaj się również z przydatnymi narzędziami do pracy z Clojure, takimi jak Leiningen: https://leiningen.org/