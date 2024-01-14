---
title:    "Clojure: Suchen und Ersetzen von Text"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

Das Suchen und Ersetzen von Text ist eine häufige Aufgabe beim Programmieren. Es ermöglicht uns, bestimmte Zeichenfolgen innerhalb von Texten zu finden und durch andere Zeichenfolgen zu ersetzen. Dies kann sowohl manuell als auch automatisiert durchgeführt werden und ist besonders nützlich, wenn wir größere Mengen an Text bearbeiten müssen.

# Wie geht's

Um Text in Clojure zu suchen und zu ersetzen, verwenden wir die Funktion `clojure.string/replace`. Diese Funktion nimmt drei Argumente an: den Text, in dem wir suchen wollen; eine reguläre Ausdrucksfolge, die spezifiziert, was wir suchen; und einen Ausdruck, durch den wir ersetzen wollen.

```Clojure
(clojure.string/replace "Hallo Welt" #"Welt" "Mars")
;; => "Hallo Mars"

(clojure.string/replace "Ich liebe Erdbeeren" #"liebe" "mag")
;; => "Ich mag Erdbeeren"
```

Wenn die zu ersetzende Zeichenfolge nicht im Text gefunden wird, bleibt der Text unverändert. Man kann auch mehrere Such- und Ersetzungsausdrücke angeben, um mehrere Zeichenfolgen gleichzeitig zu ersetzen.

```Clojure
(clojure.string/replace "Ich mag Comic Bücher" #"(Comic|Bücher)" "Manga")
;; => "Ich mag Manga Manga"
```

# Tiefer Einblick

In Clojure können wir auch benutzerdefinierte Funktionen erstellen, um Text in komplexeren Mustern zu suchen und zu ersetzen. Dafür verwenden wir die Funktion `re-pattern`, um reguläre Ausdrücke zu definieren, und `replace` für die eigentliche Ersetzung. Hier ist ein Beispiel, in dem wir alle Vokale in einem Text durch einen Unterstrich ersetzen.

```Clojure
(def text "Dies ist ein Text mit vielen Vokalen.")

(def vowel-pattern (re-pattern #"[aeiou]"))

(replace vowel-pattern text "_")
;; => "D_s _st _n T_xt m_t v_l_n _ndl_n."
```

Außerdem gibt es in Clojure auch die Funktion `replace-first`, die nur die erste Vorkommen einer Zeichenfolge ersetzt.

# Siehe auch

- [Clojure String Functions](https://clojure.github.io/clojure/clojure.string-api.html)
- [RegExr](https://regexr.com/) zur Unterstützung bei der Erstellung von regulären Ausdrücken
- [Clojure cheatsheet](https://clojure.org/api/cheatsheet) für eine Übersicht über weitere nützliche Funktionen