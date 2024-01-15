---
title:                "Tworzenie pliku tekstowego"
html_title:           "Elm: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych może być konieczne w wielu przypadkach, na przykład jeśli chcemy zapisać wyniki działania naszego programu na dysku twardym lub udostępnić je innym użytkownikom w postaci czytelnego tekstu.

## Jak to zrobić

Aby zapisać plik tekstowy w Elm, możemy użyć funkcji `File.write` z biblioteki `File`:

```Elm
File.write "wyniki.txt" "To jest przykładowy tekst do zapisania w pliku"
```

W powyższym przykładzie, po podaniu nazwy pliku oraz tekstu, funkcja `File.write` automatycznie utworzy plik i zapisze w nim podany tekst.

## Deep Dive

W przypadku bardziej skomplikowanych operacji na plikach, warto zapoznać się z modułem `File.System` w bibliotece `elm/filesystem`, który oferuje więcej możliwości, takich jak zapisywanie plików w wybranym folderze czy odczytywanie zawartości istniejącego pliku.

See Also
* https://package.elm-lang.org/packages/elm/filesystem/latest/
* https://guide.elm-lang.org/interop/file_system.html
* https://medium.com/@jkup/do-it-yourself-diy-elm-external-file-io-part-1-file-io-fd80c286a549