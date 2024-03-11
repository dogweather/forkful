---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:44.245813-07:00
description: "Zapisywanie do pliku tekstowego w Elixirze to podstawowa umiej\u0119\
  tno\u015B\u0107 dla programist\xF3w, pozwalaj\u0105ca na trwa\u0142o\u015B\u0107\
  \ danych, logowanie lub eksportowanie tre\u015Bci\u2026"
lastmod: '2024-03-11T00:14:08.234268-06:00'
model: gpt-4-0125-preview
summary: "Zapisywanie do pliku tekstowego w Elixirze to podstawowa umiej\u0119tno\u015B\
  \u0107 dla programist\xF3w, pozwalaj\u0105ca na trwa\u0142o\u015B\u0107 danych,\
  \ logowanie lub eksportowanie tre\u015Bci\u2026"
title: Pisanie pliku tekstowego
---

{{< edit_this_page >}}

## Co i dlaczego?

Zapisywanie do pliku tekstowego w Elixirze to podstawowa umiejętność dla programistów, pozwalająca na trwałość danych, logowanie lub eksportowanie treści czytelnych dla człowieka. Programiści wykonują to, aby zapisać stan aplikacji, informacje do debugowania, konfiguracje lub jakiekolwiek wymianę danych pomiędzy systemami, które preferują wszechobecny format, jakim jest tekst.

## Jak to zrobić:

Elixir ułatwia pracę z plikami dzięki wbudowanym modułom. Podstawowym sposobem na zapis do pliku jest użycie funkcji `File.write/2` lub `File.write!/2`, gdzie pierwsza zwraca krotkę `:ok` lub `:error`, a druga zgłasza błąd w przypadku niepowodzenia.

Oto prosty przykład:

```elixir
# Zapisywanie do pliku, prosta wiadomość
File.write("hello.txt", "Witaj, Świecie!")

# Po uruchomieniu kodu, tworzy plik 'hello.txt' z treścią "Witaj, Świecie!"
```

Do dopisywania do plików używa się `File.open/3` z opcjami `[:write, :append]`, a następnie `IO.binwrite/2`, aby dopisać treść:

```elixir
# Dopisywanie do pliku
{:ok, file} = File.open("hello.txt", [:write, :append])
IO.binwrite(file, "\nDodajmy kolejną linię.")
File.close(file)

# Teraz 'hello.txt' zawiera drugą linię "Dodajmy kolejną linię."
```

Jeśli pracujesz z dużymi danymi lub potrzebujesz większej kontroli nad procesem zapisu, możesz użyć modułu `Stream`, aby leniwie zapisywać dane do pliku:

```elixir
# Leniwe zapisywanie dużego zbioru danych
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Numer: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# Tworzy to 'numbers.txt', zapisując liczby od 0 do 9, każdą w nowej linii.
```

W projektach wymagających bardziej zaawansowanej obsługi plików, można również sięgnąć po biblioteki stron trzecich, takie jak `CSV`, które oferują dostosowane funkcjonalności do manipulacji plikami CSV, ale pamiętaj, dla wielu zastosowań, wbudowane możliwości Elixira są więcej niż wystarczające.
