---
title:                "Edytowanie plików w miejscu za pomocą jednolinijkowców CLI"
date:                  2024-01-27T16:20:54.677391-07:00
model:                 gpt-4-0125-preview
simple_title:         "Edytowanie plików w miejscu za pomocą jednolinijkowców CLI"

category:             "Ruby"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Edycja plików na miejscu z użyciem jednolinijkowych poleceń CLI (Command Line Interface) w Ruby pozwala na modyfikowanie plików bezpośrednio z terminala, bez potrzeby otwierania ich w edytorze, wprowadzania zmian i zapisywania z powrotem. Ta technika jest niezwykle użyteczna do szybkich modyfikacji, aktualizacji zbiorczych czy automatyzacji powtarzalnych zadań, co pozwala zaoszczędzić zarówno czas, jak i wysiłek.

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
