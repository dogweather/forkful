---
date: 2024-01-26 00:54:06.914645-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w w programowaniu polega na zarz\u0105\
  dzaniu niespodziewanymi sytuacjami\u2014rzeczami, kt\xF3re mog\u0105 p\xF3j\u015B\
  \u0107 nie tak. Programi\u015Bci robi\u0105 to, aby zapewni\u0107,\u2026"
lastmod: 2024-02-19 22:04:54.594072
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w w programowaniu polega na zarz\u0105dzaniu\
  \ niespodziewanymi sytuacjami\u2014rzeczami, kt\xF3re mog\u0105 p\xF3j\u015B\u0107\
  \ nie tak. Programi\u015Bci robi\u0105 to, aby zapewni\u0107,\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obsługa błędów w programowaniu polega na zarządzaniu niespodziewanymi sytuacjami—rzeczami, które mogą pójść nie tak. Programiści robią to, aby zapewnić, że ich programy mogą radzić sobie z tymi sytuacjami w elegancki sposób, bez awarii lub generowania błędnych wyników.

## Jak to zrobić:
Haskell radzi sobie z błędami solidnie dzięki typom takim jak `Maybe` i `Either`. Oto krótkie spojrzenie:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Dzielenie przez zero jest niedozwolone, więc zwracamy Nothing.
safeDivide x y = Just (x `div` y)  -- W przeciwnym razie, jesteśmy w porządku, zwracamy wynik w Just.

-- Zobaczmy to w działaniu:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Dla bardziej złożonej obsługi błędów w grę wchodzi `Either`:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Błąd dzielenia przez zero."  -- Tym razem, błąd niesie ze sobą wiadomość.
safeDivideEither x y = Right (x `div` y)

-- I w użyciu:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Błąd dzielenia przez zero."
```

## Dogłębna analiza
W świecie Haskella, obsługa błędów ma długą historię. Kiedyś błędy mogły powalić cały program—nic przyjemnego. System typów Haskella oferuje sposoby, aby znacznie zmniejszyć to ryzyko. Mamy `Maybe` i `Either`, ale są też inne typy jak `Exceptions` i `IO` dla różnych scenariuszy.

`Maybe` to prosta sprawa: dostajesz `Just` coś, jeśli wszystko jest w porządku, lub `Nothing`, jeśli nie jest. `Either` podnosi poziom, pozwalając zwrócić komunikat o błędzie (`Left`) lub udany wynik (`Right`).

Oba są czyste, co oznacza, że nie wpływają na świat zewnętrzny – to ważne w Haskellu. Unikamy pułapek niekontrolowanych wyjątków, które dręczą inne języki.

Dla tych, którzy nie są zadowoleni tylko z `Maybe` i `Either`, biblioteki takie jak `Control.Exception` zapewniają bardziej tradycyjne, imperatywne obsługi błędów za pomocą wyjątków. Ale zbyt liberalne ich stosowanie może komplikować sprawy, więc społeczność często trzyma się typów.

## Zobacz również
Pogłęb swą wiedzę z:

- Własnymi dokumentami Haskella: [Haskell](https://haskell.org/documentation)
- Świetne dla początkujących: ["Naucz się Haskell'a dla Wielkiego Dobra!"](http://learnyouahaskell.com/)
