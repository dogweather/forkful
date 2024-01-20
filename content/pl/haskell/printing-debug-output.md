---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Drukowanie wiadomości debugujących to sposób, w jaki programiści w trakcie pisania kodu kontrolują, jakie dane są przechowywane i przetwarzane w różnych punktach ich programów. Pomaga to w łatwym identyfikowaniu i naprawianiu błędów.

## Jak to zrobić:

Haskell oferuje kilka funkcji do drukowania wiadomości debugujących. Poniżej znajduje się przykład użycia funkcji `trace` z pakietu `Debug.Trace`.
```Haskell
import Debug.Trace

main = print (debug "Hello World")

debug :: String -> String
debug str = trace str str
```
Gdy uruchomisz ten kod, zobaczysz na konsoli napis "Hello World". 

## Pogłębione spojrzenie

Funkcja `trace` w Haskellu, jest wykorzystywana od wczesnych lat tego języka. Jest łatwa w użyciu, ale ma pewne ograniczenia. Na przykład nie powinna być używana w kodzie produkcyjnym, ponieważ jej efekty uboczne mogą prowadzić do nieprzewidywalnych wyników. 

Alternatywą dla `trace` jest używanie loggerów, które są bardziej elastyczne i oferują większe możliwości kontroli nad tym, co i jak jest drukowane. Można do nich zaliczyć takie biblioteki jak `monad-logger` czy `hslogger`.

W kontekście implementacji, `trace` jest zaimplementowana za pomocą mechanizmu `unsafePerformIO`, który pozwala na wykonanie czynności IO wewnątrz czystej funkcji. To jest jeden z powodów, dla których nie powinno się jej używać w kodzie produkcyjnym.

## Zobacz również

1. Biblioteka `Debug.Trace`: http://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html
2. Biblioteka `monad-logger`: http://hackage.haskell.org/package/monad-logger
3. Biblioteka `hslogger`: http://hackage.haskell.org/package/hslogger.