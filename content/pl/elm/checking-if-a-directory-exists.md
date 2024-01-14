---
title:                "Elm: Sprawdzanie, czy istnieje katalog"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy katalog istnieje, jest ważną częścią programowania, ponieważ umożliwia bezpieczne zarządzanie plikami i folderami na dysku. To nie tylko pomaga w uniknięciu błędów, ale także zwiększa niezawodność naszej aplikacji.

## Jak to zrobić

Aby sprawdzić istnienie katalogu w Elm, możesz użyć funkcji `Dir.exists`. Przykładowy kod wyglądałby tak:

```Elm
exists : Dir -> Task Never Bool
exists dir =
  Task.succeed True -- przykładowy kod, który zawsze zwraca wartość True
```

Jest to tylko przykład, ale pokazuje użycie tej funkcji. Pamiętaj, że funkcja ta zwraca wartość logiczną `True` lub `False` w zależności od tego, czy podana ścieżka do katalogu istnieje czy nie. 

Możesz także wyświetlić informację o istnieniu katalogu za pomocą polecenia `Debug.log` w następujący sposób:

```Elm
exists : Dir -> Task Never Bool
exists dir =
  Task.succeed True
    |> Task.andThen (\_ -> Debug.log "Katalog istnieje!")
```

To wyświetli "Katalog istnieje!" w konsoli, jeśli katalog istnieje.

## Deep Dive

Sprawdzanie istnienia katalogu może być również przydatne przy tworzeniu aplikacji, które wymagają dostępu do konkretnych plików lub folderów. Możesz także użyć funkcji `Dir.exists` do tworzenia nowych katalogów, jeśli nie istnieją.

Jednak zawsze pamiętaj o obsłudze błędów i upewnij się, że Twoja aplikacja jest przygotowana na przypadek, gdy nie będzie można dostać się do danego katalogu.

## Zobacz również

- Dokumentacja Elm o funkcjach do zarządzania plikami: https://package.elm-lang.org/packages/elm/file/latest/
- Przykładowy kod wykorzystujący `Dir.exists`: https://gist.github.com/rtfeldman/7405f63646ed613dbe00