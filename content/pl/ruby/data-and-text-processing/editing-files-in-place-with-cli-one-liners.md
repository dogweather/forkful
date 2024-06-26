---
date: 2024-01-27 16:20:54.677391-07:00
description: "Jak to zrobi\u0107: Ruby oferuje prosty spos\xF3b na edycj\u0119 plik\xF3\
  w na miejscu bezpo\u015Brednio z linii polece\u0144. Za pomoc\u0105 prze\u0142\u0105\
  cznika `-i` Ruby mo\u017Cna kaza\u0107 Ruby'emu\u2026"
lastmod: '2024-03-13T22:44:35.928652-06:00'
model: gpt-4-0125-preview
summary: "Ruby oferuje prosty spos\xF3b na edycj\u0119 plik\xF3w na miejscu bezpo\u015B\
  rednio z linii polece\u0144."
title: "Edytowanie plik\xF3w w miejscu za pomoc\u0105 jednolinijkowc\xF3w CLI"
weight: 32
---

## Jak to zrobić:
Ruby oferuje prosty sposób na edycję plików na miejscu bezpośrednio z linii poleceń. Za pomocą przełącznika `-i` Ruby można kazać Ruby'emu działać bezpośrednio na podanym(ych) pliku(ach). Przyjrzyjmy się kilku przykładom, aby zobaczyć, jak to działa w praktyce. Wyobraź sobie, że masz plik `greetings.txt` z następującą zawartością:

```
Hello, world!
Hello, Ruby!
Hello, programming!
```

I chcesz zastąpić słowo "Hello" słowem "Hi". Oto jak możesz to zrobić:

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

Po uruchomieniu tego polecenia `greetings.txt` zostanie zaktualizowany do:

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

Jeśli martwisz się o możliwość naruszenia danych, Ruby ma Cię pod kontrolą. Podając rozszerzenie do przełącznika `-i`, Ruby tworzy kopię zapasową przed wykonaniem zmian. Na przykład:

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

Teraz, wraz z edytowanym `greetings.txt`, znajdziesz w tym samym katalogu `greetings.txt.bak` z oryginalną treścią.

## Głębsze zanurzenie
Magia edycji plików na miejscu w Ruby wynika z jej połączenia podobnych do Perla możliwości przetwarzania tekstu i własnej elegancji składni Ruby. Historycznie, Perl był językiem pierwszego wyboru dla szybkiego skryptowania jednolinijkowego, zwłaszcza dla manipulacji tekstem. Ruby przyjął ten paradygmat, oferując potężne możliwości skryptowania z linii poleceń.

Alternatywy dla edycji na miejscu istnieją w innych językach, takich jak sam Perl i sed, edytor strumieniowy w systemach Unix. Każdy ma swoje mocne strony – Perl jest znany ze swojej potęgi w przetwarzaniu tekstu, podczas gdy sed jest niezrównany w swojej prostocie dla zadań edycji strumieniowej. Jednak Ruby oferuje równowagę, zapewniając solidną manipulację tekstem z bardziej czytelną i przyjazną dla użytkownika składnią, szczególnie dla osób już zaznajomionych z Ruby.

Pod kątem implementacji, edycja plików na miejscu w Ruby działa poprzez zmianę nazwy oryginalnego pliku, tworzenie nowego z oryginalną nazwą pliku, a następnie zapisywanie zmian do tego nowego pliku podczas czytania z przemianowanego oryginału. To podejście zapewnia atomowość operacji; cały plik jest przetwarzany pomyślnie albo żadne zmiany nie są wprowadzane, chroniąc integralność danych podczas procesu edycji. Ten mechanizm, w połączeniu z obsługą wyjątków Ruby, zapewnia również odporność na przerwy, takie jak awarie zasilania czy zabijanie procesu, gwarantując, że co najmniej kopia zapasowa pozostanie nienaruszona.

Podsumowując, edycja plików na miejscu w Ruby jest świadectwem jej użyteczności jako języka skryptowego, oferując mieszankę mocy, prostoty i elegancji do zadań manipulacji tekstem bezpośrednio z linii poleceń.
