---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja ciągu to proces zastępowania miejsca docelowego w łańcuchu znaków rzeczywistą wartością zmiennych. Programiści robią to, aby dynamicznie zmieniać treść ciągu, co zwiększa moc i adaptacyjność naszego kodu.

## Jak zrobić:

Zasada jest prosta. W ciele ciągu używamy symbolu `#{}` do otaczania zmiennych, które chcemy interpolować. 
Zobaczmy to na przykładzie:

```ruby
imię = "Krzysztof"
powitanie = "Cześć, #{imię}!"
puts powitanie
```

Wyjście będzie wyglądać tak:

```ruby
"Cześć, Krzysztof!"
```

## Na głębokości:

Interpolacja ciągu jest z nami od dawna i jest powszechnie stosowana w wielu językach programowania. W Ruby, jest to natywne zachowanie ciągu. Możemy używać interpolacji ciągu w dowolnym miejscu, gdzie potrzebujemy dynamicznie obsługiwać ciągi.

Różne są alternatywne metody, takie jak sklejanie łańcuchów za pomocą `+`, ale interpolacja ciągu jest zdecydowanie bardziej elegancka i wydajna.

Szczególne jest to, jak Ruby interpretuje wartości w ramach `#{}`. Wywołuje metodę `.to_s` na wartości, aby przekształcić ją w ciąg. Niezależnie od typu wartości - czy to string, liczba, czy obiekt - zostanie prawidłowo zinterpretowana jako ciąg.

## Zobacz też:

1. Dokumentacja Ruby: [Interpolacja ciągu](https://ruby-doc.org/core-2.2.0/doc/syntax/literals_rdoc.html#label-Strings)
 
Uczyć się, praktykować i kodować!