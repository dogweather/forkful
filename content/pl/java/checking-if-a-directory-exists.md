---
title:                "Sprawdzanie istnienia folderu"
html_title:           "Java: Sprawdzanie istnienia folderu"
simple_title:         "Sprawdzanie istnienia folderu"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzanie, czy istnieje katalog, jest procesem, w którym program sprawdza, czy dany katalog rzeczywiście istnieje w systemie plików. Programiści często wykonują to sprawdzenie, aby upewnić się, że będą mogli odwołać się do katalogu przed wykonaniem dalszych operacji na plikach wewnątrz niego.

## Jak to zrobić?

W Javie można użyć metody ```File.exists()```, która zwraca wartość logiczną (true lub false) w zależności od tego, czy katalog istnieje czy nie. Przykładowy kod wygląda następująco:

```
File directory = new File("ścieżka/do/katalogu");
if (directory.exists()) {
    System.out.println("Katalog istnieje!");
} else {
    System.out.println("Katalog nie istnieje!");
}
```

Przykładowy wynik:

```
Katalog istnieje!
```

## Wchodząc w szczegóły

Sprawdzanie, czy katalog istnieje, jest ważną częścią programowania, ponieważ pozwala uniknąć błędów, jeśli dana operacja na plikach musi być wykonana w danym katalogu. Alternatywą dla metody ```File.exists()``` jest użycie metody ```Files.exists()``` z pakietu ```java.nio.file```. Jest to bardziej nowoczesne i wydajniejsze rozwiązanie, ale wymaga nieco więcej kodu.

W Javie 8 i wcześniejszych wersjach, aby sprawdzić, czy katalog istnieje, można było użyć metody ```File.isDirectory()``` i sprawdzić, czy zwraca wartość true. Jednak ta metoda nie była dokładna i mogła zwrócić wartość false, nawet gdy katalog istniał w systemie plików.

## Zobacz także

- Dokumentacja Javy dotycząca klasy ```java.io.File```: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Poradnik na temat operacji na plikach w Javie: https://www.tutorialspoint.com/java/java_files_io.htm