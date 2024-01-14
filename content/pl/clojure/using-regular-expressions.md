---
title:                "Clojure: Używanie wyrażeń regularnych"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Działanie wyrażeń regularnych może wydawać się skomplikowane i niepotrzebne, ale w rzeczywistości jest to bardzo przydatne narzędzie dla programistów. Pozwala ono na szybkie i precyzyjne przetwarzanie tekstów, znajdowanie określonych wzorców oraz manipulację nimi.

## Jak to zrobić

Aby użyć wyrażeń regularnych w języku Clojure, należy użyć funkcji `re-seq` lub `re-matches`. Przykładowo, jeśli chcemy znaleźć wszystkie wystąpienia słowa "programowanie" w tekście, możemy użyć wyrażenia regularnego `#"programowanie"`. Następnie, możemy użyć funkcji `re-seq` do przetworzenia tekstu i otrzymać listę wszystkich pasujących fragmentów. 

```Clojure
(def tekst "Kiedy potrzebujemy lekkiego odprężenia, najlepiej zająć się czymś przyjemnym, np. programowaniem.")
(re-seq #"\w+" tekst)

; Output: ["Kiedy" "potrzebujemy" "lekkiego" "odprężenia" "najlepiej" "zająć" "się" "czymś" "przyjemnym" "np" "programowaniem"]
```

Aby zastąpić fragment tekstu, można użyć funkcji `re-find`, a następnie funkcji `clojure.string/replace`. W poniższym przykładzie zastępujemy wszystkie wystąpienia słowa "programowaniem" słowem "kodowaniem".

```Clojure
(def tekst "Kiedy potrzebujemy lekkiego odprężenia, najlepiej zająć się czymś przyjemnym, np. programowanie.")
(re-find #"\w+" tekst)
(clojure.string/replace tekst #"programowaniem" "kodowaniem")

; Output: "Kiedy potrzebujemy lekkiego odprężenia, najlepiej zająć się czymś przyjemnym, np. kodowaniem."
```

## Głębszy Przegląd

Wyrażenia regularne pozwalają na używanie zaawansowanych wzorców, takich jak klasy znaków, kwantyfikatory czy grupy. Dzięki temu można dokładnie określić, jakich fragmentów tekstu szukamy. Ważnym elementem jest wyrażenie `^` oznaczające początek tekstu oraz wyrażenie `$` oznaczające koniec tekstu, które można wykorzystać do szukania ciągów znaków na początku lub końcu wyrażenia.

## Zobacz też

- Dokumentacja Clojure dotycząca wyrażeń regularnych: https://clojuredocs.org/clojure.core/re-seq
- Przydatny tutorial odnośnie wyrażeń regularnych w języku Clojure: https://www.braveclojure.com/regular-expressions/