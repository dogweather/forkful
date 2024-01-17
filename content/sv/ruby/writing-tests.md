---
title:                "Skriva tester"
html_title:           "Ruby: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-tests.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att skriva tester handlar om att skriva kod som kontrollerar att din övriga kod fungerar som den ska. Det är en viktig del av programmering eftersom det hjälper dig hitta och förhindra buggar innan de når produktionen.

# Hur gör man:
I Ruby finns det en populär testsvit som heter MiniTest. För att använda den, lägg bara till ```require 'minitest/autorun'``` längst upp i din testfil och skriv dina tester inuti ```class TestNågonting < Minitest::Test ... end```. För att köra testerna, skriv bara ```ruby testfilnamn.rb``` i terminalen.

# Djupdykning:
Det är viktigt att skriva tester eftersom det hjälper dig hitta och förhindra buggar tidigt. Dessutom hjälper det dig att förstå din kod bättre och ger dig mer självförtroende när du programmerar. En annan metod för att skriva tester är Behavior-Driven Development (BDD) där man fokuserar på hur koden beter sig istället för hur den är skriven.

# Se även:
- [MiniTest dokumentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/minitest/rdoc/MiniTest.html)
- [Artikel om BDD](https://medium.com/@KerrySheldon/behaviour-driven-development-and-rspec-ruby-on-rails-1c55601388c9)
- [Exempel på ett testetui i Ruby](https://www.qaiter.com/testify/)