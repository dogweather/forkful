---
date: 2024-01-26 04:17:46.038921-07:00
description: "Interaktywna konsola, czyli REPL (Read-Eval-Print Loop - Cykl Czytaj-Wykonaj-Wydrukuj),\
  \ pozwala testowa\u0107 kod w czasie rzeczywistym. Programi\u015Bci u\u017Cywaj\u0105\
  \u2026"
lastmod: '2024-02-25T18:49:34.309823-07:00'
model: gpt-4-0125-preview
summary: "Interaktywna konsola, czyli REPL (Read-Eval-Print Loop - Cykl Czytaj-Wykonaj-Wydrukuj),\
  \ pozwala testowa\u0107 kod w czasie rzeczywistym. Programi\u015Bci u\u017Cywaj\u0105\
  \u2026"
title: Korzystanie z interaktywnego shella (REPL)
---

{{< edit_this_page >}}

## Co i dlaczego?
Interaktywna konsola, czyli REPL (Read-Eval-Print Loop - Cykl Czytaj-Wykonaj-Wydrukuj), pozwala testować kod w czasie rzeczywistym. Programiści używają jej do eksperymentowania, debugowania i poznawania subtelności Ruby'ego bez tworzenia pełnoprawnych skryptów.

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
