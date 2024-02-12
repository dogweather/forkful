---
title:                "Pisanie do standardowego błędu"
aliases: - /pl/ruby/writing-to-standard-error.md
date:                  2024-02-03T19:34:26.376529-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie do standardowego błędu"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego błędu (stderr) w Ruby polega na kierowaniu komunikatów o błędach lub informacji diagnostycznych do oddzielnego strumienia wyjściowego, różnego od standardowego wyjścia (stdout). Programiści robią to, aby odróżnić regularne wyjście programu od błędów i informacji debugujących, co ułatwia diagnozowanie problemów i analizę logów.

## Jak to zrobić:
Standardowa biblioteka Ruby'ego zapewnia prosty sposób na zapis do stderr za pomocą `$stderr` lub `STDERR`. Nie potrzebujesz bibliotek stron trzecich do tej podstawowej operacji.

### Zapisywanie prostej wiadomości do stderr:
```ruby
$stderr.puts "Błąd: Nie znaleziono pliku."
# Lub równoważnie
STDERR.puts "Błąd: Nie znaleziono pliku."
```
Przykładowe wyjście (do stderr):
```
Błąd: Nie znaleziono pliku.
```

### Przekierowywanie stderr do pliku:
```ruby
File.open('error.log', 'w') do |plik|
  STDERR.reopen(plik)
  STDERR.puts "Nie udało się otworzyć konfiguracji."
end
```
Ten fragment kodu przekierowuje stderr do pliku o nazwie `error.log`, a wszystkie kolejne zapisane błędy będą tam wyjściowane aż do zresetowania przekierowania stderr lub zakończenia programu.

### Użycie stderr z obsługą wyjątków:
```ruby
begin
  # Symulacja operacji, która może się nie powieść, np. otwarcie pliku
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "Wystąpił wyjątek: #{e.message}"
end
```
Przykładowe wyjście (do stderr):
```
Wystąpił wyjątek: No such file or directory @ rb_sysopen - nonexistent_file.txt
```

Chociaż wbudowane metody Ruby'ego do pisania do stderr wystarczają dla wielu zastosowań, dla bardziej złożonych potrzeb logowania można rozważyć użycie standardowej biblioteki `logger` lub zewnętrznych gemów takich jak `Log4r`. Zapewniają one konfigurowalne mechanizmy logowania, w tym poziomy ważności, formatowanie i możliwość zapisu do różnych wyjść, w tym do plików, e-maili i innych.
