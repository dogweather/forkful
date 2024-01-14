---
title:    "Ruby: Zamiana napisu na duże litery"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym wpisie na blogu omówimy temat kapitalizacji ciągów tekstu w języku Ruby. Dowiesz się, dlaczego jest to ważne, jak to zrobić oraz jak działa ta funkcja w głębszym ujęciu.

## Jak to zrobić

Oto prosty przykład, jak użyć metody `capitalize` w Ruby, aby przekształcić pierwszą literę ciągu tekstu na wielką:

```Ruby
name = "kasia"
puts name.capitalize
```

Output:

```
Kasia
```

Proste, prawda? Metoda `capitalize` zmienia tylko pierwszą literę ciągu, pozostałe pozostają bez zmian.

A co jeśli chcemy, aby każde słowo w ciągu zaczynało się wielką literą? W takim przypadku możemy użyć metody `split` i `map`:

```Ruby
sentence = "lubie jesc pizze"
puts sentence.split.map(&:capitalize).join(" ")
```

Output:

```
Lubie Jesc Pizze
```

W tym przykładzie, najpierw dzielimy ciąg na pojedyncze słowa, następnie mapujemy każde słowo i stosujemy do nich metodę `capitalize`, a na końcu łączymy słowa z powrotem w całość.

## Deep Dive

W rzeczywistości, metoda `capitalize` jest tylko jedną z wielu metod, które pozwalają na manipulację ciągami tekstu w języku Ruby. Podczas gdy metoda ta jest przydatna do prostych zastosowań, warto również zapoznać się z innymi metodami, takimi jak: `upcase`, `downcase`, `swapcase` i `capitalize!`. Opcja z wykrzyknikiem (`!`) oznacza, że zmiana zostanie dokonana w miejscu, bez tworzenia nowego obiektu.

Możliwości manipulacji ciągami tekstu w języku Ruby są naprawdę szerokie i warto poświęcić trochę czasu na zgłębienie ich poznania, aby móc wykorzystać je w swoich projektach.

## Zobacz również
- Ruby documentation: https://ruby-doc.org/core-3.0.0/String.html
- RubyGuides: https://www.rubyguides.com/2019/05/ruby-string-methods/