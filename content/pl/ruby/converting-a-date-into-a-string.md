---
title:    "Ruby: Konwersja daty na ciąg znaków"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego?

Czasami, kiedy pracujesz z danymi w swoim programie, musisz zamienić datę z formatu obiektu na format, który łatwiej jest odczytać dla człowieka. Na przykład może to być potrzebne, gdy chcesz wyświetlić datę w czytelny sposób w interfejsie użytkownika lub zapisać ją w pliku tekstowym.

## Jak to zrobić?

Język Ruby oferuje prosty sposób na przekonwertowanie daty na łańcuch znaków za pomocą metody `strftime`. W poniższym przykładzie używamy formatu `"%d/%m/%Y"`, który przedstawia datę w postaci `DD/MM/RRRR`.

```Ruby
date = Time.new(2021, 8, 23)
puts date.strftime("%d/%m/%Y")
# => 23/08/2021
```

Jeśli chcesz wyświetlić więcej informacji, można użyć różnych specyfikatorów formatu, takich jak `%a` dla skróconego dnia tygodnia lub `%b` dla skróconego miesiąca. Więcej informacji na ten temat można znaleźć w dokumentacji języka Ruby.

## Głębszy zanurzenie

Kiedy używasz metody `strftime`, ważne jest, aby upewnić się, że data, którą chcesz przekonwertować, jest właściwym obiektem `Time` lub `DateTime`. Może to być nieoczekiwanie wymagane, jeśli masz obiekt daty w innym formacie, na przykład jako łańcuch znaków lub integer.

W takim przypadku najlepiej przekonwertować go do obiektu `Time` lub `DateTime` za pomocą metod `parse` lub `strptime` i dopiero potem użyć `strftime` do wyświetlenia daty w wybranym formacie.

## Zobacz też

- [Dokumentacja języka Ruby](https://www.ruby-lang.org/pl/documentation/)
- [Przewodnik po funkcji strftime](https://smalldata.tech/blog/2018/8/21/ruby-strftime?lang=pl) (język angielski)