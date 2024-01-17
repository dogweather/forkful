---
title:                "Łączenie ciągów znaków"
html_title:           "Ruby: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Konkatenacja ciągów znaków jest procesem łączenia różnych ciągów znaków w jeden dłuższy ciąg. Programiści często wykonują tę operację, aby tworzyć bardziej złożone i użyteczne wyrazy lub zdania, które mogą być wykorzystane w ich kodzie.

## Jak to zrobić:
Konkatenacja ciągów znaków w języku Ruby jest bardzo prosta. Wystarczy użyć operatora "+" pomiędzy dwoma zmiennymi zawierającymi ciągi znaków, aby je połączyć w jeden. Przykładowy kod wyglądałby tak:

```Ruby
imie = "John"
nazwisko = "Smith"
imie_i_nazwisko = imie + " " + nazwisko
puts imie_i_nazwisko
```

Na wyjściu otrzymalibyśmy "John Smith", ponieważ operator "+" działa tak samo jak efekt konkatenacji. Możemy też połączyć stały ciąg znaków zmiennym lub ze sobą, jak w tym przykładzie:

```Ruby
ulubione_zwierze = "Panda"
puts "Moje ulubione zwierzę to: " + ulubione_zwierze + "!"
```

Na wyjściu otrzymalibyśmy "Moje ulubione zwierzę to: Panda!".

## Wgląd w dłuższą historię:
Konkatenacja ciągów znaków jest powszechnie używaną operacją w wielu językach programowania. Termin ten wywodzi się z łacińskiego słowa "konkatenare", co oznacza "łączyć". W Ruby konkatenacja jest często wykorzystywana w celu tworzenia dynamicznych wyrażeń lub w celu łączenia różnych fragmentów kodu.

Alternatywą dla operatora "+" jest metoda "concat", która działa w podobny sposób. Jednak, w przeciwieństwie do operatora, "concat" nie modyfikuje piersiwej zmiennej, tylko zwraca nowy ciąg.

## Zobacz także:
Dla więcej informacji na temat konkatenacji ciągów znaków w języku Ruby, możesz zajrzeć do dokumentacji online: https://ruby-doc.org/core-2.7.1/String.html#method-i-2B.
Jeśli interesuje cię temat funkcji i operacji na ciągach znaków w ogólności, warto również poznać moduł "String" w Ruby: https://ruby-doc.org/core-2.7.1/String.html.