---
title:                "Wydobywanie podciągów"
html_title:           "Ruby: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyciąganie podciągów to proces wyodrębniania fragmentów danych z większego ciągu znaków. Programiści często wykorzystują tę technikę, aby uporządkować i manipulować tekstami w swoich programach.

## Jak to zrobić:
```Ruby
example_text = "Witaj w świecie programowania!"
puts example_text[6..-1] # Output => w świecie programowania!
```

W powyższym przykładzie użyto metody ```[]```, która pozwala na wycięcie fragmentu tekstu z wybranego zakresu. Warto zauważyć, że w Ruby indeksy zaczynają się od zera, a ```-1``` oznacza ostatni element w tekście.

## Wnikliwe spojrzenie:
1. Kontekst historyczny: Wyciąganie podciągów jest powszechną częścią języka Ruby, który został stworzony w 1993 roku przez Yukihiro Matsumoto.
2. Alternatywy: Istnieje wiele innych metod operujących na tekstach w Ruby, takich jak: ```slice```, ```substring```, ```scan```. Każda z nich ma jednak swoje specyficzne zastosowanie.
3. Szczegóły implementacji: Wyciąganie podciągów wykorzystuje indeksy zamiast iteracji po każdym znaku, co pomaga oszczędzić czas i zasoby programu.

## Zobacz też:
[Oficjalna dokumentacja Ruby o metodzie '[]'](https://ruby-doc.org/core-2.7.1/String.html#method-i-5B-5D)

[Blokwisko o wyciąganiu podciągów w Ruby](https://www.blokwisko.agh.edu.pl/post/ruby-wyrazenia-regularne-i-wyciaganie-duzych-ciagow-znakow)