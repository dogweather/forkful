---
date: 2024-01-27 20:35:17.262703-07:00
description: "Jak to zrobi\u0107: Ruby oferuje kilka metod generowania losowych liczb,\
  \ g\u0142\xF3wnie za po\u015Brednictwem klasy `Random`. #."
lastmod: '2024-03-13T22:44:35.926664-06:00'
model: gpt-4-0125-preview
summary: "Ruby oferuje kilka metod generowania losowych liczb, g\u0142\xF3wnie za\
  \ po\u015Brednictwem klasy `Random`."
title: Generowanie liczb losowych
weight: 12
---

## Jak to zrobić:
Ruby oferuje kilka metod generowania losowych liczb, głównie za pośrednictwem klasy `Random`.

### Podstawowa losowa liczba
Aby wygenerować podstawową losową liczbę:

```Ruby
puts rand(10) # Generuje losową liczbę między 0 a 9
```

### Losowa liczba w zakresie
Aby uzyskać losową liczbę w określonym zakresie:

```Ruby
puts rand(1..10) # Generuje losową liczbę między 1 a 10
```

### Używanie klasy Random
Aby stworzyć powtarzalną sekwencję losowych liczb, można użyć klasy `Random` z ziarnem.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # Generuje przewidywalną "losową" liczbę
```

### Generowanie losowego elementu tablicy
Wybierz losowy element z tablicy:

```Ruby
colors = ["red", "blue", "green", "yellow"]
puts colors.sample # Losowo wybiera element z tablicy
```

### Przykładowe wyjście:
Każdy fragment kodu powyżej, po uruchomieniu, wyprodukuje różne wyniki ze względu na ich losowy charakter. Na przykład, `rand(10)` może wyjść `7`, podczas gdy `colors.sample` może wyjść `"green"`.

## Dogłębna analiza
Koncept generowania losowych liczb w informatyce jest paradoksalny, ponieważ komputery działają według deterministycznych instrukcji. Wczesne metody w dużej mierze opierały się na zewnętrznym wejściu, aby osiągnąć nieprzewidywalność. Losowość Ruby opiera się na algorytmie Mersenne Twister, pseudo-losowym generatorze liczb znany z jego ogromnego okresu i równomiernego rozkładu, co czyni go bardzo odpowiednim do aplikacji wymagających wysokiej jakości losowości.

Chociaż wbudowane metody Ruby doskonale spełniają większość potrzeb, mogą nie wystarczyć do wszystkich celów kryptograficznych, ponieważ przewidywalność pseudo-losowych liczb może być wadą. Dla bezpieczeństwa kryptograficznego, programiści Ruby mogą badać biblioteki takie jak `OpenSSL::Random`, które są zaprojektowane, aby produkować kryptograficznie bezpieczne losowe liczby, zapewniając wyższą nieprzewidywalność dla wrażliwych aplikacji.
