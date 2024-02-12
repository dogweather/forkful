---
title:                "Tworzenie pliku tymczasowego"
aliases: - /pl/ruby/creating-a-temporary-file.md
date:                  2024-01-20T17:41:32.401596-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tworzenie pliku tymczasowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
## Co i Dlaczego?

Tworzenie plików tymczasowych pozwala na bezpieczne przechowywanie danych tylko na czas trwania procesu. Programiści używają ich, gdy potrzebują miejsca na dysku, które zniknie bez śladu po zakończeniu pracy.

## How to:
## Jak to zrobić:

Ruby ma wbudowaną bibliotekę `Tempfile` do obsługi plików tymczasowych. Użyj jej tak:

```ruby
require 'tempfile'

Tempfile.create('moj_plik') do |plik|
  plik.write('Witaj, świat!')
  puts "Plik tymczasowy istnieje: #{plik.path}"
end

# Po wyjściu z bloku, plik jest automatycznie usunięty.
puts "Plik tymczasowy został usunięty? #{!File.exist?(plik.path)}"
```

Sample output:
```
Plik tymczasowy istnieje: /tmp/moj_plik20180319-4721-x3shzn
Plik tymczasowy został usunięty? true
```

## Deep Dive:
## Wgłębiając się:

W Ruby, pliki tymczasowe działają dzięki `Tempfile`, który z kolei korzysta z klas niższego poziomu jak `File` i `Dir`. W przeszłości, zanim `Tempfile` stała się częścią standardowej biblioteki, tworzenie plików tymczasowych było bardziej żmudne i podatne na błędy.

Alternatywnie, możesz stworzyć własne zarządzanie plikami tymczasowymi, używając klas `File` i `Dir`, ale pamiętaj o manualnym usuwaniu pliku. `Tempfile` automatycznie usuwa plik po zamknięciu obiektu lub gdy proces kończy działanie.

Pod kątem implementacji, `Tempfile` tworzy unikalne nazwy dla plików, aby zapobiec konfliktom, gdy wielu użytkowników lub procesów tworzy pliki tymczasowe jednocześnie. Zawiera też mechanizmy zabezpieczające przed atakami typu race condition.

## See Also:
## Zobacz również:

- Ruby's File and Dir classes for manual file handling: [https://ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html), [https://ruby-doc.org/core/Dir.html](https://ruby-doc.org/core/Dir.html)
- Official Ruby programming language site: [https://www.ruby-lang.org/pl/](https://www.ruby-lang.org/pl/)
