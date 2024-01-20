---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Wydobycie Podciągów w Clojure: Co, Dlaczego i Jak?

## Co i Dlaczego?
"Extraction of substrings" to proces wyodrębniania mniejszych ciągów znaków z większego ciągu znaków. Programiści robią to, aby manipulować danymi i przeprowadzać analizę tekstu.

## Jak to zrobić:
Clojure oferuje kilka funkcji do wyodrębniania podciągów. Oto kilka przykładów:

```clojure
;; Używając funkcji 'subs' do wyodrębnienia podciągu.
(let [str "Witaj, świecie!"]
  (subs str 0 5))  
;; Wynik: "Witaj" 

;; Używając funkcji 'subs' do wyodrębnienia podciągu od określonego indeksu.
(let [str "Witaj, świecie!"]
  (subs str 7))  
;; Wynik: "świecie!"
```

## Głębsze Zagłębienie
Historia: Ogólna idea wyodrębniania podciągów sięga początków informatyki - jest to uniwersalny koncept obecny w wielu językach programowania.

Alternatywy: Inne Clojure funkcje, takie jak slit-at lub partition, mogą równie dobrze służyć do przetwarzania ciągów, zależnie od Twojego konkretnego przypadku użycia.

Implementacja: Warto zauważyć, że indeksy w Clojure, jak w wielu językach programowania, zaczynają się od 0, co oznacza, że pierwszym znakiem w ciągu jest element na pozycji 0.

## Zobacz również
Dla dalszego rozwinięcia tematu polecam następujące źródła:
1. Oficjalna dokumentacja Clojure na temat obsługi ciągów: [Clojure Strings](https://clojure.org/reference/strings) 
2. 'Clojure for the Brave and True' - doskonałe źródło dla początkujących: [Odczytywanie danych z ciągów](https://www.braveclojure.com/reading-data-from-string/) 
3. Stackoverflow - Zawsze pełne przydatnych wątków na temat programowania: [Clojure tags on Stackoverflow](https://stackoverflow.com/questions/tagged/clojure)