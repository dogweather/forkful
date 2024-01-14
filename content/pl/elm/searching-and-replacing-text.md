---
title:    "Elm: Wyszukiwanie i zamienianie tekstu"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego warto poszukać i zamienić tekst w swoim programie w Elm? W tym artykule opowiem Ci o kilku powodach, dla których jest to przydatne narzędzie dla każdego programisty.

Głównym powodem, dla którego warto używać funkcji `String.replace` w Elm, jest możliwość szybkiego i łatwego zmieniania tekstu w wielu plikach jednocześnie. Wystarczy jedna komenda, aby zastąpić wszędzie dowolne wyrażenie lub słowo bez konieczności szukania i poprawiania każdego wystąpienia oddzielnie.

## Jak to zrobić

W Elm możemy używać `String.replace` w prosty sposób za pomocą funkcji `String.replace old new text`, gdzie `old` to szukane wyrażenie, `new` to zamiennik, a `text` to tekst, w którym chcemy dokonać zmiany. Poniżej znajduje się przykładowy kod w Elm oraz wynik działania:

```Elm
String.replace "pszenica" "ryż" "Lubię jeść pszenicę na śniadanie."
```

```
"Lubię jeść ryż na śniadanie."
```

Możemy również użyć `String.replace` w połączeniu z funkcją `List.map` dla większej liczby plików. Przykładowy kod poniżej zamienia nazwę programu Shopify na Spotify we wszystkich plikach w liście `files`:

```Elm
List.map (\file -> String.replace "Shopify" "Spotify" file) files

-- Plik 1: "Moja aplikacja Shopify jest niesamowita."
-- Plik 2: "Kupiłem kilka produktów na Shopify."
```

```
-- Plik 1: "Moja aplikacja Spotify jest niesamowita."
-- Plik 2: "Kupiłem kilka produktów na Spotify."
```

## Dogłębny przegląd

Funkcja `String.replace` w Elm oferuje również wiele opcji dostosowywania podczas wyszukiwania i zamieniania tekstu. Możemy użyć dowolnego wyrażenia regularnego jako `old`, co daje możliwość zaawansowanego przeszukiwania i zastępowania. Możemy również użyć opcji `limit`, aby określić maksymalną liczbę wystąpień, które chcemy zastąpić.

Funkcja `String.replace` jest również bardzo wydajna, ponieważ stosuje specjalne optymalizacje w celu uniknięcia niepotrzebnych iteracji przez tekst. W ten sposób może być używana nawet w dużych i złożonych projektach bez problemów wydajnościowych.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcji `String.replace` w Elm, polecam zapoznać się z oficjalną dokumentacją oraz innymi przydatnymi artykułami na temat programowania w Elm:

- https://guide.elm-lang.org/strings/
- https://www.elm-tutorial.org/en/03-subs-cmds/04-request.html
- https://medium.com/@evancz/string-literals-are-not-utf-8-266f6919e26f

Dzięki wykorzystaniu funkcji `String.replace` w Elm, możesz zaoszczędzić czas i wysiłek podczas zmieniania tekstu w swoim programie. Jest to proste i wydajne narzędzie, które może być bardzo przydatne w różnych scenariuszach. Dlatego warto poznać je lepiej i wykorzystać w swoim kodzie.