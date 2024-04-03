---
date: 2024-01-26 04:17:46.038921-07:00
description: "Jak to zrobi\u0107: REPL Ruby'ego nazywa si\u0119 IRB (Interaktywne\
  \ Ruby). Wskocz i wypr\xF3buj Ruby prosto z terminala."
lastmod: '2024-03-13T22:44:35.934009-06:00'
model: gpt-4-0125-preview
summary: "REPL Ruby'ego nazywa si\u0119 IRB (Interaktywne Ruby)."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak to zrobić:
REPL Ruby'ego nazywa się IRB (Interaktywne Ruby). Wskocz i wypróbuj Ruby prosto z terminala:

```Ruby
irb
2.7.0 :001 > puts "Witaj, świecie Ruby!"
Witaj, świecie Ruby!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Zagłębiając się
Wprowadzony w Ruby 1.8, IRB jest podstawą dla Rubyistów. Jest inspirowany interaktywnymi konsolami Lisp i Pythona, łącząc eksperymentowanie z natychmiastowym feedbackiem. Alternatywy takie jak Pry oferują więcej funkcji, takich jak podświetlanie składni i bardziej zaawansowane środowisko debugowania. Sam IRB jest prosty, ale może być rozbudowany o gemy takie jak 'irbtools', aby rozszerzyć funkcjonalność. Sposób, w jaki IRB obsługuje pętlę czytaj-wykonaj-wydrukuj, polega na odczytywaniu każdej linii wejścia, ocenianiu jej jako kod Ruby'ego, a następnie drukowaniu wyniku, zapętlając ten proces do wyjścia.

## Zobacz również
- [IRB Ruby'ego](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [Gem irbtools](https://github.com/janlelis/irbtools)
