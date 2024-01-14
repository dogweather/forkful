---
title:                "Haskell: Tworzenie tymczasowego pliku"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest ważną częścią wielu aplikacji, szczególnie w języku Haskell. Pozwala to na przechowywanie danych tymczasowych, które nie są potrzebne w trakcie dłuższego działania programu lub nie wymagają długoterminowego przechowywania. W tym artykule dowiesz się, dlaczego warto używać plików tymczasowych w swoim kodzie Haskell.

## Jak to zrobić

Tworzenie plików tymczasowych w języku Haskell jest bardzo proste. Można to zrobić za pomocą funkcji `withTempFile` z modułu `System.IO.Temp`. Przykładowy kod wyglądałby następująco:

```Haskell
import System.IO.Temp

main :: IO ()
main = withTempFile "temp/" "example.txt" $ \tempFile -> do
    putStrLn $ "Utworzono plik tymczasowy: " ++ tempFile
    writeFile tempFile "To jest przykładowy tekst"
```

W powyższym przykładzie, funkcja `withTempFile` tworzy plik o podanej nazwie w określonym katalogu (`temp/`), a następnie wykonuje akcję z parametrem `tempFile`, który jest nazwą utworzonego pliku. W tym przypadku, do pliku zostaje zapisany przykładowy tekst.

Po uruchomieniu tego programu, zostanie wyświetlony komunikat informujący o utworzeniu pliku tymczasowego oraz zostanie utworzony plik `example.txt` w katalogu `temp/` z zapisanym tekstem. Po zakończeniu działania programu, plik tymczasowy zostanie automatycznie usunięty.

## Głębszy wgląd

Tworzenie plików tymczasowych jest nie tylko przydatne, ale także ważne z punktu widzenia bezpieczeństwa. Dzięki temu, nie będziemy zanieczyszczać naszej przestrzeni dyskowej niepotrzebnymi plikami, co może prowadzić do niepożądanych konsekwencji. Ponadto, korzystanie z funkcji `withTempFile` pozwala na uniknięcie błędów związanych z ręcznym usuwaniem plików tymczasowych, ponieważ zostaną one automatycznie usunięte po zakończeniu działania programu.

Warto również zauważyć, że `withTempFile` jest tylko jedną z wielu funkcji z modułu `System.IO.Temp`, który pozwala na wygodne i bezpieczne tworzenie, otwieranie i usuwanie plików tymczasowych w języku Haskell. Warto zapoznać się z całą dokumentacją tego modułu, aby poznać wszystkie dostępne opcje.

## Zobacz także

- [Dokumentacja modułu`System.IO.Temp` (ang.)](https://hackage.haskell.org/package/temporary-1.3.2.0/docs/System-IO-Temp.html)
- [Oficjalna strona języka Haskell (pol.)](https://www.haskell.org/)