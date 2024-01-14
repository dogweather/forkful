---
title:    "Java: Tworzenie tymczasowego pliku"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego może być przydatne w wielu sytuacjach programistycznych. Jest to czynność, która pozwala na tworzenie plików, które są potrzebne tylko w określonych sytuacjach lub są używane tylko przez krótki czas.

## Jak to zrobić

Aby utworzyć tymczasowy plik w języku Java, można skorzystać z metody `createTempFile()` z klasy `File`. Przykładowy kod wyglądałby następująco:

```Java
File tempFile = File.createTempFile("plik_tymczasowy", ".tmp");
```

Pierwszy argument metody `createTempFile()` określa nazwę pliku, a drugi jego rozszerzenie. Plik zostanie automatycznie utworzony w folderze, w którym działa program. Należy również pamiętać o obsłużeniu wyjątków w przypadku niepowodzenia tworzenia pliku.

Możliwe jest również ustalenie własnego folderu, w którym zostanie utworzony tymczasowy plik. W tym celu należy użyć metody `createTempFile()` z klasy `File` z trzema argumentami: nazwą pliku, rozszerzeniem oraz ścieżką do wybranego folderu.

## Głębsze wgląd

Tworzone pliki tymczasowe mają unikalne nazwy i są automatycznie usuwane po zakończeniu działania programu. Można jednak również określić własne parametry dotyczące usuwania pliku. Służą do tego metody klasy `File` takie jak `deleteOnExit()` czy `delete()`, które pozwalają na decyzję o usunięciu pliku w określonym momencie.

## Zobacz również

- Dokumentacja klasy `File`: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Informacje o obsłudze wyjątków: https://www.javatpoint.com/exception-handling-in-java