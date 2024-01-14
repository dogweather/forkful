---
title:                "Haskell: Pisanie do standardowego błędu"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego życia. Czy jesteś początkującym programistą czy też doświadczonym specjalistą, z pewnością spotkałeś się z problemem debugowania swojego kodu. W takim przypadku warto wiedzieć, jak pisać do standardowego błędu (standard error) w języku Haskell, aby ułatwić sobie pracę i szybko znaleźć ewentualne błędy w swoim kodzie.

## Jak to zrobić

Aby pisać do standardowego błędu w Haskellu, używamy funkcji `hPutStrLn` z modułu `System.IO`. Funkcja ta przyjmuje dwa parametry - uchwyt (handle) i napis (string) do wypisania. Uchwyt `stderr` jest dostępny w module `System.IO` i pozwala na wypisanie tekstu równolegle z użyciem funkcji `putStrLn`. 

```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "To jest przykładowy błąd"
    putStrLn "Inny tekst"
```

Output:
```
To jest przykładowy błąd
Inny tekst
```

W powyższym przykładzie widzimy, że wypisanie tekstu do standardowego błędu nie przerywa działania programu i nie wpływa na wyświetlenie tekstu w standardowym wyjściu (standard output).

## Głębszy zanurzenie

Jeśli chcielibyśmy pisać więcej informacji do standardowego błędu, możemy użyc funkcji `hPutStr` zamiast `hPutStrLn`. Funkcja `hPutStr` wypisze do standardowego błędu wszystko, co znajduje się w napisie, bez dodatkowego znaku nowej linii na końcu. 

```Haskell
import System.IO

main :: IO ()
main = do
    hPutStr stderr "To jest pierwsza linia błędu"
    hPutStr stderr "A to jest druga linia błędu"
```

Output:
```
To jest pierwsza linia błęduA to jest druga linia błędu
```

W ten sposób możemy dokładniej kontrolować wyjście do standardowego błędu w naszych programach.

## Zobacz również

- [Dokumentacja funkcji hPutStrLn](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html#v:hPutStrLn)
- [Dokumentacja funkcji hPutStr](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html#v:hPutStr)
- [Treningowy kurs programowania w języku Haskell](https://learnyouahaskell.com/)