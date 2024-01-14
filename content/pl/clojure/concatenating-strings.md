---
title:    "Clojure: Łączenie ciągów znaków"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś nowym programistą w świecie Clojure, może się zastanawiasz po co w ogóle trzeba łączyć stringi. Czy to w ogóle ważna część programowania? Dlaczego nie można po prostu używać pojedynczych stringów? Ten wpis jest dla Ciebie, ponieważ wyjaśnię dlaczego łączenie stringów jest nieodłączną częścią wielu programów napisanych w Clojure.

## Jak To Zrobić

Do łączenia stringów w Clojure używamy funkcji ```str``` lub operatora ```~```. Sprawdźmy to na prostym przykładzie:

```Clojure
(str "Hello" " " "World")
;; Wynik: "Hello World"

(println (str "This is" " a " "test"))
;; Wynik: This is a test

(str "1" " + 2 = " (+ 1 2))
;; Wynik: "1 + 2 = 3"
```

W pierwszym przykładzie łączymy stringi "Hello", spacje i "World" w jeden string. W drugim przykładzie łączymy stringi za pomocą operatora ```~``` i wypisujemy wynik na konsoli. W ostatnim przykładzie łączymy stringi z wyrażeniem matematycznym za pomocą funkcji ```str```.

## Głębsza Analiza

W naszych przykładach użyliśmy funkcji ```str``` lub operatora ```~``` do połączenia stringów. Jednakże, jeśli uważnie przyjrzysz się pierwszemu przykładowi, zauważysz, że w rzeczywistości używaliśmy funkcji ```str``` tylko raz, a następnie łączyliśmy stringi za pomocą operatora ```~```. Dlaczego tak się stało? Otóż operator ```~``` jest rozwinięciem dla funkcji ```str``` i jest bardziej czytelne w przypadku składania większej ilości stringów. Dzięki temu, nasze kody są czytelniejsze i łatwiejsze do zrozumienia.

Kolejną ważną rzeczą do zauważenia jest to, że Clojure traktuje stringi jako sekwencje i posiada funkcję ```clojure.string/join```, która umożliwia wklejenie separatora między elementami sekwencji. Przyjrzyjmy się temu na przykładzie:

```Clojure
(clojure.string/join ", " ["a" "b" "c"])
;; Wynik: "a, b, c"
```

Inną użyteczną funkcją jest ```clojure.string/join```, która dokonuje konkatenacji znaków z wycinaniem elementów sekwencji.

```Clojure
(clojure.string/join " " (take 2 "clojure"))
;; Wynik: "cl"
```

## Zobacz też
- [Clojure Docs on Strings](https://clojuredocs.org/clojure.string)
- [Concatenating Strings in Clojure](https://www.tutorialspoint.com/clojure/clojure_strings.htm)
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)