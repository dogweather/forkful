---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:59.015234-07:00
description: "Jak to zrobi\u0107: Fish nie obs\u0142uguje natywnie tablic asocjacyjnych\
  \ tak jak Bash 4+, ale mo\u017Cna osi\u0105gn\u0105\u0107 podobn\u0105 funkcjonalno\u015B\
  \u0107, u\u017Cywaj\u0105c kombinacji list i\u2026"
lastmod: '2024-03-13T22:44:35.831843-06:00'
model: gpt-4-0125-preview
summary: "Fish nie obs\u0142uguje natywnie tablic asocjacyjnych tak jak Bash 4+, ale\
  \ mo\u017Cna osi\u0105gn\u0105\u0107 podobn\u0105 funkcjonalno\u015B\u0107, u\u017C\
  ywaj\u0105c kombinacji list i manipulacji ci\u0105gami znak\xF3w."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
Fish nie obsługuje natywnie tablic asocjacyjnych tak jak Bash 4+, ale można osiągnąć podobną funkcjonalność, używając kombinacji list i manipulacji ciągami znaków. Oto jak je naśladować:

Najpierw ustawiając elementy "tablicy asocjacyjnej" oddzielnie:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

Aby uzyskać dostęp do elementu, po prostu odwołaj się do niego bezpośrednio:

```Fish Shell
echo $food_color_apple
# Wyjście: red
```

Jeśli potrzebujesz iterować po nich, użyj pętli for z uwzględnieniem konwencji nazewnictwa:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Wyjście:
# red
# yellow
```

Dla tych, którym brakuje funkcji Bash `${!array[@]}`, by uzyskać wszystkie klucze, można przechowywać klucze na osobnej liście:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'jest' $food_color_$key
end
# Wyjście:
# apple jest red
# banana jest yellow
```

## Głębsze zanurzenie
Prawdziwe tablice asocjacyjne, jak w innych językach skryptowych, nie są jeszcze częścią podejścia Fish. Obejście pokazane wykorzystuje możliwości manipulacji ciągami znaków i list Fish, aby stworzyć strukturę pseudo-tablicy asocjacyjnej. Chociaż to działa, nie jest to tak czyste lub wolne od błędów, jak gdyby wsparcie dla wbudowanych tablic asocjacyjnych było dostępne. Inne powłoki, takie jak Bash i Zsh, zapewniają wbudowaną funkcjonalność tablic asocjacyjnych, co skutkuje bardziej prostym, czytelnym kodem. Jednak filozofia projektowania Fish ma na celu prostotę i przyjazność dla użytkownika, możliwe kosztem takich funkcji. Obejście zaspokaja większość potrzeb, ale warto obserwować ewolucję Fish Shell - jego twórcy aktywnie ulepszają i dodają funkcje na podstawie opinii społeczności.
