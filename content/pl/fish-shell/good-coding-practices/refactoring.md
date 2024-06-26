---
date: 2024-01-26 01:18:23.912725-07:00
description: "Jak to zrobi\u0107: Wyobra\u017A sobie, \u017Ce masz skrypt, kt\xF3\
  ry z czasem mocno uro\u015Bl. Zaczyna\u0142 prosto, ale teraz jest to bestia rozprzestrzeniaj\u0105\
  ca macki logiki. Oto\u2026"
lastmod: '2024-03-13T22:44:35.849979-06:00'
model: gpt-4-0125-preview
summary: "Wyobra\u017A sobie, \u017Ce masz skrypt, kt\xF3ry z czasem mocno uro\u015B\
  l."
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:
Wyobraź sobie, że masz skrypt, który z czasem mocno urośl. Zaczynał prosto, ale teraz jest to bestia rozprzestrzeniająca macki logiki. Oto przykład refaktoryzacji funkcji, aby była bardziej zrozumiała i wydajna:

Przed refaktoryzacją:
```fish
function old_and_clunky
    set color (cat ~/.config/fish/color_theme)
    if test "$color" = 'blue'
        echo 'Ustawiono niebieski motyw!'
    else if test "$color" = 'red'
        echo 'Ustawiono czerwony motyw!'
    else
        echo 'Ustawiono domyślny motyw!'
    end
end
```

Po refaktoryzacji:
```fish
function set_theme_color
    set theme_color (cat ~/.config/fish/color_theme)
    switch $theme_color
        case blue
            echo 'Ustawiono niebieski motyw!'
        case red
            echo 'Ustawiono czerwony motyw!'
        default
            echo 'Ustawiono domyślny motyw!'
    end
end
```
Refaktoryzacja poprawiła nazwę funkcji, aby lepiej opisywała jej cel oraz zastąpiła łańcuch instrukcji if-else bardziej zgrabnym poleceniem `switch`.

Przykładowy wynik:
```
Ustawiono niebieski motyw!
```

## Dogłębne rozważania
Refaktoryzacja została po raz pierwszy szczegółowo opisana w książce Martina Fowlera "Refaktoryzacja: Ulepszanie projektu istniejącego kodu". Książka przedstawiła strukturalne podejście do poprawiania kodu bez pisania nowej funkcjonalności. Od tego czasu wprowadzono wiele technik refaktoryzacji, a koncepcja stała się podstawową częścią nowoczesnego rozwoju oprogramowania.

W środowisku Fish Shell refaktoryzacja może wyglądać nieco inaczej niż w innych kontekstach programistycznych ze względu na jej specjalistyczną składnię i charakter poleceń konsolowych. Alternatywy dla refaktoryzacji skryptów w Fish mogą obejmować przenosiny do innego języka powłoki lub używanie zewnętrznych narzędzi do bardziej zaawansowanego zarządzania skryptami. Jednak zachowanie natywnej składni Fish często oznacza lepszą integrację z funkcjami powłoki i bardziej uproszczone doświadczenie ogólne.

Przy refaktoryzacji w Fish Shell masz do czynienia głównie z funkcjami i poleceniami, w przeciwieństwie do klas lub modułów o szerokim zasięgu, które są powszechne w innych językach. Ta szczegółowość może sprawić, że zadanie refaktoryzacji jest bardziej bezpośrednim i konkretnym procesem, ale również podkreśla znaczenie jasnego, zwięzłego i łatwego do utrzymania kodu.

## Zobacz również
- Strona o refaktoryzacji Martina Fowlera: [https://refactoring.com/](https://refactoring.com/)
- Oficjalna dokumentacja Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
