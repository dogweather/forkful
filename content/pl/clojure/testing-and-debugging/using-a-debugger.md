---
date: 2024-01-26 03:48:26.454487-07:00
description: "Jak to zrobi\u0107: Clojure opiera si\u0119 na Maszynie Wirtualnej Javy\
  \ (JVM), wi\u0119c wiele debugowania odbywa si\u0119 za pomoc\u0105 narz\u0119dzi\
  \ Java. Jednym z takich narz\u0119dzi\u2026"
lastmod: '2024-03-13T22:44:35.000635-06:00'
model: gpt-4-0125-preview
summary: "Clojure opiera si\u0119 na Maszynie Wirtualnej Javy (JVM), wi\u0119c wiele\
  \ debugowania odbywa si\u0119 za pomoc\u0105 narz\u0119dzi Java."
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
Clojure opiera się na Maszynie Wirtualnej Javy (JVM), więc wiele debugowania odbywa się za pomocą narzędzi Java. Jednym z takich narzędzi jest `CIDER`, potężny pakiet do rozwoju Clojure w Emacs, który posiada solidne możliwości debugowania. Zanurkujmy:

```clojure
;; Najpierw, podłącz się do projektu Clojure w Emacs za pomocą CIDER
M-x cider-jack-in

;; Ustaw punkt przerwania
;; Przejdź do linii w swoim kodzie Clojure, którą chcesz zbadać i
;; naciśnij "C-c M-b" lub wykonaj:
M-x cider-debug-defun-at-point

;; Gdy kod zostanie uruchomiony, trafisz na przerwanie. CIDER zapyta Cię:
;; 1. n, aby przejść do następnego logicznego kroku w wykonaniu,
;; 2. c, aby kontynuować wykonanie do następnego punktu przerwania,
;; 3. q, aby zakończyć debugowanie.

;; Inspekcja zmiennych lokalnych w punkcie przerwania
;; Będąc na przerwaniu, wpisz:
locals

;; Zobaczysz listę zmiennych lokalnych i ich wartości wydrukowanych w minibufferze.
```
Przykładowy wynik może wyglądać tak:
```clojure
{:x 10, :y 20, :result 200}
```

## Głębsze zanurzenie
Debugger to narzędzie stworzone na początkach informatyki. Termin "bug" został ukuty na wczesnym etapie rozwoju informatyki, gdy rzeczywisty owad spowodował błąd, zwierając obwód w maszynie.

Chociaż `CIDER` jest świetny dla entuzjastów Emacs, istnieją alternatywy do debugowania Clojure. Na przykład, używanie IntelliJ z pluginem Cursive może zapewnić doświadczenie debugowania bardziej oparte na interfejsie użytkownika GUI. Dodatkowo, można użyć wbudowanego Leiningen lub tools.deps do kontrolowania przepływu procesu podczas debugowania.

W tle, te debuggery często manipulują kodami bajtów, przeprowadzają ewaluacje w dedykowanych sesjach nREPL i oferują inspekcję śladów stosu. Wykorzystują możliwości leżące u podstaw JVM, czerpiąc z bogactwa frameworków debugowania Java.

## Zobacz także
- [Dokumentacja Debugera CIDER](https://docs.cider.mx/cider/debugging/debugger.html)
- [Debugger Cursive](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen dla Automatyzacji i Debugowania](https://leiningen.org/)
- [tools.deps.alpha dla większej kontroli](https://github.com/clojure/tools.deps.alpha)
