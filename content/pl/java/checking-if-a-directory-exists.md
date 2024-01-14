---
title:    "Java: Sprawdzanie istnienia katalogu"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie czy istnieje katalog jest ważnym aspektem programowania w Javie. Za pomocą tej funkcji można wykonać wiele różnych operacji, takich jak tworzenie, usuwanie i modyfikacja plików i folderów. Jest to niezbędne w wielu przypadkach, a więc warto się z tym zapoznać.

## Jak to zrobić

Aby sprawdzić czy dany katalog istnieje, należy użyć metody `exists()` z klasy `File`. Przykładowe użycie można zobaczyć poniżej:

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        // utworzenie obiektu reprezentującego katalog
        File directory = new File("mojekatalog");

        // sprawdzenie czy katalog istnieje
        if(directory.exists()){
            System.out.println("Katalog istnieje");
        } else {
            System.out.println("Katalog nie istnieje");
        }
    }
}
```

Powyższy kod wyświetli informację czy katalog `mojekatalog` istnieje czy nie. Można też użyć metody `isDirectory()` aby upewnić się, że sprawdzany plik jest właśnie katalogiem.

## Wnikliwa analiza

Głównym zadaniem funkcji `exists()` jest sprawdzenie czy dany plik lub katalog istnieje w systemie plików. Jest to bardzo przydatna funkcja w wielu sytuacjach. Na przykład, przed utworzeniem nowego pliku, warto sprawdzić czy nie istnieje już plik o takiej samej nazwie. W przypadku usuwania pliku, dobrze jest najpierw sprawdzić czy faktycznie istnieje, aby uniknąć błędów.

W przypadku, gdy program wykonuje wiele operacji na plikach, ważne jest również sprawdzenie czy ścieżka do pliku jest prawidłowa, a katalogi w których będzie się znajdować plik już istnieją. W takiej sytuacji, funkcja `exists()` jest nieoceniona.

## Zobacz też

- [Metoda exists() w dokumentacji Javy](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#exists())
- [Tutorial o pracy z plikami i katalogami w Javie](https://www.tutorialspoint.com/java/java_files_io.htm)
- [Jak sprawdzić czy plik istnieje w Javie? - artykuł na blogu devstyle.pl](https://devstyle.pl/2015/02/24/jak-sprawdzic-czy-plik-istnieje-w-javie/)

Dzięki funkcji `exists()` możemy w prosty sposób sprawdzić czy dany katalog istnieje w systemie plików. Jest to niezbędne w wielu przypadkach i warto znać tę funkcję.