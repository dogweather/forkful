---
title:                "Tworzenie pliku tekstowego"
html_title:           "Ruby: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Tworzenie plików tekstowych jest częstym zadaniem podczas programowania w Ruby. Pliki tekstowe pozwalają na przechowywanie dużych ilości danych w łatwy do przetwarzania sposób. Jest to również doskonały sposób na interakcję z użytkownikiem poprzez wyświetlanie informacji w oknie konsoli.

## Jak to zrobić?

Aby utworzyć plik tekstowy w Ruby, użyjemy metody `File.open("nazwa_pliku.txt", "w")`, która otwiera plik tekstowy w trybie zapisu. Następnie możemy wykorzystać metodę `puts` do zapisania danych w pliku oraz `close` aby zamknąć plik. Przykładowy kod wyglądałby następująco:

```Ruby
File.open("moj_plik.txt", "w") do |plik|
  plik.puts "To jest pierwsza linia tekstu"
  plik.puts "A to jest druga linia"
  plik.puts "Możemy dodawać tyle linii, ile chcemy"
end
```

W powyższym przykładzie wykorzystujemy blokowy zapis metody `File.open`, który automatycznie zamknie plik po jej wykonaniu. Oczywiście, możemy również wywołać metodę `close` w późniejszym etapie. 

## Głębsza rzeźba

Podczas otwierania pliku tekstowego w trybie zapisu, istnieje kilka opcji, które możemy przekazać jako drugi argument metody `File.open`. Należą do nich:

- "w" - dla trybu zapisu. Tworzy nowy plik jeśli nie istnieje lub kasuje zawartość istniejącego pliku.
- "w+" - pozwala na zapis i odczyt z pliku
- "a" - dla trybu dopisywania. Dodaje nowe dane na końcu istniejącego pliku.
- "a+" - pozwala na dopisywanie oraz odczyt z pliku

Ponadto, możemy również wykorzystać metody `File.write` lub `File.puts` do bezpośredniego zapisu danych do pliku bez potrzeby otwierania go w trybie zapisu. 

## Zobacz również

- Oficjalna dokumentacja Ruby: https://ruby-lang.org/pl/
- Przewodnik Ruby dla początkujących: https://rubyschool.us/
- Kursy programowania w Ruby: https://codecademy.com/learn/learn-ruby