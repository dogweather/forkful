---
title:                "Pisanie do standardowego błędu"
html_title:           "Haskell: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest ważną częścią programowania w Haskellu, ponieważ pozwala programiście na obsługę wyjątków i wyświetlanie błędów w przejrzysty sposób. Jest to niezbędne przy pracy nad wymagającymi i skomplikowanymi projektami.

## Jak to zrobić

Pisanie do standardowego błędu w Haskellu jest bardzo proste. Wystarczy skorzystać z funkcji `hPutStrLn` z modułu `System.IO`. Przykładowy kod wyglądałby następująco:

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "To jest błąd!"
```

Po uruchomieniu tego kodu, wyjście zostanie wyświetlone w standardowym strumieniu błędów, co w większości przypadków jest widoczne w konsoli.

Możemy także wykorzystać tę funkcję w połączeniu z operatorem `>>` do wywoływania funkcji jedna po drugiej:

```Haskell
main = hPutStrLn stderr "Pierwszy błąd!" >> hPutStrLn stderr "Drugi błąd!"
```

W tym przypadku, będą wyświetlone dwa błędy pod sobą w standardowym strumieniu błędów.

## Wnikliwa analiza

Funkcja `hPutStrLn` jest zdefiniowana jako `hPutStrLn :: Handle -> String -> IO ()` i przyjmuje dwa argumenty - uchwyt do określonego strumienia (w tym przypadku `stderr`) oraz łańcuch znaków, który ma być wyświetlony. Dlatego też funkcja ta jest często używana do wyświetlania błędów podczas obsługi wyjątków.

Dodatkowo, funkcja `hPutStrLn` musi być wywołana wewnątrz monady `IO`, co oznacza, że może ona modyfikować stan programu, w tym wypadku wyświetlić błąd w standardowym strumieniu błędów.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o pisaniu do standardowego błędu w Haskellu, polecamy zapoznanie się z dokumentacją modułu `System.IO` oraz artykułem "Debugging in Haskell" na stronie [Haskell.org](https://www.haskell.org/).