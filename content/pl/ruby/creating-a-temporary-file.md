---
title:    "Ruby: Tworzenie pliku tymczasowego"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego jest przydatne w wielu sytuacjach, zwłaszcza podczas pisania programów, które wymagają tymczasowego przechowywania danych. Może to być również przydatne przy testowaniu nowych funkcji lub rozwiązywaniu problemów.

## Jak to zrobić

```Ruby
# Standardowy sposób tworzenia tymczasowego pliku w Ruby
temp_file = Tempfile.new("nazwa_pliku") 
puts temp_file.path
```

```Ruby
# Wczytywanie danych do tymczasowego pliku i odczytywanie ich
temp_file = Tempfile.new("nazwa_pliku")
temp_file.write("To jest przykładowy tekst")
temp_file.rewind
puts temp_file.read(20)
```
Output:
`To jest przykładowy tekst`

```Ruby
# Tworzenie tymczasowego pliku w określonej lokalizacji
temp_file = Tempfile.new("nazwa_pliku", "ścieżka/do/lokalizacji")
```

## Głębsza analiza

Tworzenie tymczasowego pliku w Ruby jest bardzo proste i wygodne. Możemy to zrobić za pomocą wbudowanej klasy `Tempfile`, która automatycznie zarządza usuwaniem pliku po jego zamknięciu.

`Tempfile.new` przyjmuje dwa argumenty - nazwę pliku oraz opcjonalną ścieżkę do lokalizacji. Jeśli nie podamy ścieżki, plik zostanie utworzony w bieżącym katalogu. 

Głównym powodem, dla którego wiele programistów wykorzystuje tymczasowe pliki, jest bezpieczeństwo danych. Używając tymczasowego pliku, możemy mieć pewność, że nasze dane nie zostaną przypadkowo nadpisane lub usunięte. Po zakończeniu pracy z plikiem, zostanie on automatycznie usunięty, więc nie musimy martwić się o jego dalsze przetwarzanie.

## Zobacz także

- [Dokumentacja klasy Tempfile w Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html)
- [Krótki przewodnik o tworzeniu plików tymczasowych w Ruby](https://www.rubyguides.com/2015/05/working-with-temporary-files-in-ruby)
- [Artykuł o stosowaniu plików tymczasowych w programowaniu w Ruby](https://www.honeybadger.io/blog/ruby-temporary-file)