---
title:                "Korzystanie z interaktywnego shella (REPL)"
aliases:
- /pl/javascript/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:15:42.127682-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Interaktywne powłoki, czyli REPL-y (Read-Eval-Print Loops - Pętle Czytaj-Wykonaj-Wypisz), pozwalają na bieżąco uruchamiać kod, testować funkcje, algorytmy lub bawić się pomysłami. Są jak kartki do szybkich notatek w programowaniu, szybkie i brudne, bez konieczności ustawiania pełnego środowiska deweloperskiego.

## Jak to zrobić:
Node.js jest dostarczany z REPL dostępnym poprzez terminal. Otwórz go, a będziesz gotowy do działania. Oto mała próbka:

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

Proste, prawda? Definiuj zmienne, funkcje lub uruchamiaj pętle. Kiedy skończysz, `.exit` zabierze cię z powrotem do rzeczywistości.

## Dogłębna analiza
REPL-e istnieją od lat 60. XX wieku – LISP zapoczątkował ten koncept. Ideą było zapewnienie natychmiastowego feedbacku dla programisty. Alternatywy? Poza REPL Node.js, są też konsolowe narzędzia przeglądarki takie jak Chrome DevTools, online piaskownice takie jak JSFiddle, czy pełne IDE takie jak VSCode z interaktywnymi placami zabaw.

Pod kapotem, przepływ pracy REPL zazwyczaj obejmuje: 
1. Odczyt wejścia
2. Kompilację i wykonanie kodu
3. Wypisanie wyniku
4. Powrót do początku

To prosta, aczkolwiek skuteczna pętla, która miała ogromny wpływ na interaktywne programowanie.

## Zobacz także
- [Dokumentacja REPL Node.js](https://nodejs.org/api/repl.html)
- [Wprowadzenie do modułów JavaScript na REPL-ach od Mozilli](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
