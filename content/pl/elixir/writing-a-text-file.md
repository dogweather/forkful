---
title:    "Elixir: Tworzenie pliku tekstowego"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie tekstu do pliku jest nieodłączną częścią procesu pisania oprogramowania. Bez możliwości zapisywania danych na dysku, nasz program nie mógłby działać w sposób trwały i efektywny. Ponadto, pisanie do pliku może być również przydatne w celu zapisania wyników działania programu lub generowania raportów.

## Jak

Poniżej przedstawimy przykłady kodów z użyciem języka Elixir, aby pokazać jak w prosty sposób możemy zapisać tekst do pliku.

```Elixir
# Tworzenie pliku i zapisywanie do niego tekstu
File.write("plik.txt", "Ten tekst zostanie zapisany do pliku")

# Dodawanie tekstu do istniejącego pliku
File.append("plik.txt", "Ten tekst zostanie dodany na końcu pliku")

# Odczytywanie tekstu z pliku
File.read("plik.txt")  
```

Po wykonaniu powyższych operacji, nasz plik "plik.txt" będzie zawierał następujące dane:

Ten tekst zostanie zapisany do pliku
Ten tekst zostanie dodany na końcu pliku

## Deep Dive

Kiedy piszemy do pliku w języku Elixir, jest to tak naprawdę wywoływanie funkcji z modułu "File". Funkcja "write" przyjmuje dwa argumenty - nazwę pliku i same dane, które chcemy zapisać. Natomiast funkcja "append" dodaje tekst do już istniejącego pliku, a funkcja "read" odczytuje zawartość pliku i zwraca ją jako binarny ciąg znaków.

Warto również wiedzieć, że funkcje te posiadają dodatkowe opcje, takie jak tryb zapisu (nadpisywania lub dodawania do już istniejącego pliku), formatowanie danych lub ustawienie prawa dostępu do pliku.

## Zobacz również

- [Dokumentacja języka Elixir dotycząca pisania do pliku] (https://hexdocs.pm/elixir/File.html)
- [Poradnik dla początkujących w języku Elixir] (https://elixir-lang.org/getting-started/)
- [Inne przydatne funkcje z modułu "File"] (https://hexdocs.pm/elixir/File.html#content)

Dzięki wykorzystaniu powyższych funkcji z modułu "File", możemy łatwo i szybko zapisywać dane do pliku w języku Elixir. Oczywiście, istnieją również inne sposoby na zapisywanie danych, takie jak wykorzystanie baz danych czy zewnętrznych bibliotek, jednak pisanie tekstu do pliku pozostaje podstawą dla wielu programistów. Należy pamiętać o zapewnieniu odpowiednich uprawnień dostępu do pliku oraz o odpowiednim formatowaniu danych, aby zapewnić czytelność i bezpieczeństwo naszego kodu.