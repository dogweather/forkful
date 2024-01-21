---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:35:19.132333-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? - Co i dlaczego?
Łączenie łańcuchów znaków, czyli "string concatenation", to proces, w którym sklejamy dwa lub więcej tekstów w jeden ciągły ciąg znaków. Programiści robią to, żeby budować wiadomości, tworzyć dynamiczne dane czy też po prostu składają razem różne kawałki informacji.

## How to - Jak to zrobić?
Łączenie stringów w Gleamie jest proste. Używamy operatora `++` albo funkcji `string.append`. Oto przykłady:

```gleam
pub fn main() {
  let greeting = "Cześć, "
  let name = "Ania!"
  let welcome = greeting ++ name
  let farewell = string.append("Do widzenia, ", name)
  
  assert welcome == "Cześć, Ania!"
  assert farewell == "Do widzenia, Ania!"
}
```

Uruchomienie powyższego kodu nie zwraca żadnego błędu, co znaczy, że nasze teksty zostały połączone poprawnie.

## Deep Dive - Głębsze spojrzenie
W przeszłości języki jak C wymagały skomplikowanych operacji do łączenia stringów, w tym kopii pamięci i obsługi wskaźników. W językach wysokiego poziomu, jak Gleam, to dużo prostsze, ale wciąż musimy być świadomi wydajności. Duże ilości łączeń mogą spowolnić program, bo każde łączenie tworzy nowy string, co wiąże się z alokacją pamięci.

Alternatywą dla operatora `++` może być użycie funkcji z rodziny `string` czy przygotowanie całego ciągu w jednym literale przy użyciu interpolacji stringów - jednak Gleam w obecnej wersji interpolacji nie wspiera. Kiedy łączymy znacznie większe ilości danych, możemy skorzystać z typów takich jak `StringBuilder` w innych językach, które są zoptymalizowane pod kątem takich operacji.

Pod względem implementacji, Gleam wykonuje łączenie stringów w sposób bezpieczny i prewidywany, a sam język promuje niemutowalność, co stwarza dobre warunki dla równoczesnego wykonywania operacji bez obawy o race conditions.

## See Also - Zobacz też
- Oficjalna dokumentacja Gleam o stringach: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)
- Skuteczną praktykę w łączeniu tekstów, temat "More Efficient String Concatenation in Gleam": [Forum Dyskusyjne Gleam](https://github.com/gleam-lang/gleam/discussions)
- "Effective String Concatenation in Functional Languages" dla porównania metod w różnych językach funkcyjnych: [http://www.vasinov.com/blog/](http://www.vasinov.com/blog/)