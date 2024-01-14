---
title:    "Haskell: Pisanie do standardowego błędu"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć! Dziś poznamy się bliżej z jednym z najważniejszych aspektów pisania w Haskellu - pisaniem do standardowego błędu. Niektórzy mówią, że nie ma w życiu nic pewniejszego niż śmierć i podatki, ale ja bym dodał do tego jeszcze jeden pewnik - pisaniu do stderr. Wszak nie jest tajemnicą, że każdy programista jest urodzonym ekspertem w generowaniu błędów!

## Jak to zrobić

Aby pisać do standardowego błędu w Haskellu, musimy skorzystać z funkcji `hPutStrLn` z modułu `System.IO`. Oto przykładowe użycie tej funkcji wraz z oczekiwanym wynikiem:

```Haskell
import System.IO

main = do
    hPutStrLn stderr "To jest przykładowy błąd!"
```

Wywołanie tej funkcji spowoduje wyświetlenie napisu "To jest przykładowy błąd!" na standardowym błędzie. Proste, prawda?

## Głębsze wytłumaczenie

Ważnym aspektem pisania do stderr jest również zachowanie oczekiwanej kolejności wyjścia w naszym programie. W przypadku błędu, który zostanie wyrzucony przez funkcję `hPutStrLn`, program będzie kontynuował działanie i wypisze wyjście na stdout. Dzięki temu nasza aplikacja może kontynuować pracę i nawet w przypadku błędu przekazać użytkownikowi niezbędne informacje.

Warto również zaznaczyć, że wyjście do stderr należy wypisywać w synchroniczny sposób, czyli w odpowiedniej kolejności. Niektóre funkcje modułu `System.IO` (np. `hPutStr`) wywołują operacje asynchroniczne, co może spowodować nieprawidłową kolejność wypisywanych komunikatów.

## Zobacz także
- Dokumentacja modułu `System.IO`: https://hackage.haskell.org/package/base/docs/System-IO.html
- Przykłady użycia `hPutStrLn`: https://www.stackage.org/haddock/lts/system-io-1.0.0.3/System-IO.html#v:hPutStrLn