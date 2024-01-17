---
title:                "Pobieranie strony internetowej"
html_title:           "Clojure: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?

Pobieranie strony internetowej to proces, w którym program pobiera dane ze strony internetowej, takie jak tekst, obrazy i multimedia, i wyświetla je w czytelnej formie. Programiści często pobierają strony internetowe, aby przeglądać zawartość lub pobierać dane do analizy lub przetwarzania.

# Jak to zrobić:

```Clojure
(require '[clojure.java.io :as io])  
(io/copy (io/input-stream "https://www.example.com") (io/output-stream "output.html"))
```

W powyższym kodzie importujemy bibliotekę `clojure.java.io`, a następnie wywołujemy funkcję `copy` z dwoma argumentami: `input-stream` dla adresu strony internetowej i `output-stream` dla pliku, w którym chcemy zapisać pobrane dane. Po uruchomieniu tego kodu, otrzymamy plik `output.html` zawierający pobraną stronę internetową.

# W pogłębieniu:

## Kontekst historyczny:

Wcześniej programiści często używali języka Java do pobierania stron internetowych, ale dzięki rozwojowi Clojure powstały proste i wydajne metody do wykonania tego zadania.

## Alternatywy:

Alternatywą dla użycia biblioteki `clojure.java.io` jest użycie specjalistycznych bibliotek, takich jak `clj-webdriver` lub `jython`, ale te wymagają więcej doświadczenia i są skomplikowane dla początkujących.

## Szczegóły implementacyjne:

W bibliotece `clojure.java.io` funkcja `copy` wykorzystuje metody `URLConnection` z języka Java, aby nawiązać połączenie z adres