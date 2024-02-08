---
title:                "Refaktoryzacja"
aliases:
- pl/haskell/refactoring.md
date:                  2024-01-26T01:18:45.956466-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to proces modyfikowania kodu bez zmiany jego zewnętrznego zachowania. Chodzi o uporządkowanie i oczyszczenie kodu, aby stał się łatwiejszy do odczytania, utrzymania i rozszerzenia. Może również pomóc wyeliminować błędy i poprawić wydajność.

## Jak to zrobić:
Załóżmy, że masz fragment kodu w Haskellu, który powtarza się częściej niż twoja ulubiona piosenka. Oto krótkie spojrzenie na to, jak można to zrefaktoryzować, używając funkcji.

Przed refaktoryzacją:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  putStrLn $ "Klient: " ++ customer
  putStrLn $ "Suma: " ++ show total
  putStrLn $ "Przedmiot: " ++ item
```

Po nieco refaktoryzacji:

```haskell
printDetail :: String -> String -> IO ()
printDetail label value = putStrLn $ label ++ ": " ++ value

printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  printDetail "Klient" customer
  printDetail "Suma" (show total)
  printDetail "Przedmiot" item

-- Przykładowe wyjście:
-- Klient: Alice
-- Suma: $42.00
-- Przedmiot: Przewodnik programowania w Haskellu
```

Jak widać, wyodrębniając wspólny wzorzec do osobnej funkcji `printDetail`, unikamy powtórzeń i czynimy `printInvoice` bardziej przejrzystym i łatwiejszym do zarządzania.

## Głębsze zanurzenie
Kiedy Haskell pojawił się na scenie pod koniec lat 80., było jasne, że paradygmat funkcyjny może przynieść świeże powietrze do praktyk programowania. Z biegiem czasu, refaktoryzacja w Haskellu jest szczególnie elegancka dzięki temu, że funkcje są obywatelami pierwszej klasy oraz dzięki silnemu, statycznemu systemowi typów. Można przeprowadzać refaktoryzację bez obaw, że zepsuje się aplikację, ponieważ kompilator ma to pod kontrolą.

Alternatywami dla manualnej refaktoryzacji mogą być narzędzia automatyczne, choć funkcjonalny charakter i bezpieczeństwo typów w Haskellu mogą czasami sprawić, że są one mniej powszechne w porównaniu z innymi językami. Pod względem implementacyjnym ważne jest korzystanie z funkcji Haskellu takich jak funkcje wyższego rzędu, czystość i niemutowalność, aby refaktoryzacja przebiegała płynniej.

Refaktoryzacje takie jak "Wyodrębnij funkcję", właśnie przedstawione, są powszechne, ale można również wykonywać "Wstaw funkcję inline", "Zmień nazwę zmiennej" i "Zmień sygnaturę funkcji" z pewnością siebie dzięki systemowi typów. Potężne wnioskowanie o typach w Haskellu czasami może wyłapać błędy, które przeszłyby niezauważone w innych językach.

## Zobacz również
Aby zgłębić refaktoryzację w Haskellu, sięgnij po książkę "Refaktoryzacja: Ulepszanie istniejącego kodu" autorstwa Martina Fowlera, gdzie koncepcje są uniwersalnie stosowane. Sprawdź narzędzie hlint dla automatycznych wskazówek dotyczących ulepszania kodu w Haskellu. Odwiedź także wiki Haskell (https://wiki.haskell.org/Refaktoryzacja) po spostrzeżenia społeczności i dalsze czytanie.
