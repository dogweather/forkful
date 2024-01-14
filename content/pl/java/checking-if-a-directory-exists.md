---
title:    "Java: Sprawdzanie istnienia folderu"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Dlaczego sprawdzić, czy istnieje katalog

Sprawdzenie, czy katalog istnieje jest ważnym elementem w programowaniu Java, ponieważ pozwala na uniknięcie błędów związanych ze ścieżką dostępu do plików. Dzięki temu możemy mieć pewność, że wykonywane operacje na plikach będą działać prawidłowo.

# Jak to zrobić

Sprawdzenie, czy dany katalog istnieje w Javie jest bardzo proste. Wystarczy wykorzystać klasę "Files" oraz metodę "exists", która zwróci wartość "true" lub "false", w zależności od tego czy dany katalog faktycznie istnieje czy nie.

```Java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class SprawdzanieKatalogu {

    public static void main(String[] args) {

        // Ustawienie ścieżki do katalogu, który chcemy sprawdzić
        Path katalog = Paths.get("C:/Users/Ja/Programy");

        // Wywołanie metody exists i przypisanie wyniku do zmiennej
        boolean czyIstnieje = Files.exists(katalog);

        // Wyświetlenie odpowiedniego komunikatu w zależności od wyniku
        if (czyIstnieje) {
            System.out.println("Katalog istnieje!");
        } else {
            System.out.println("Katalog nie istnieje!");
        }
    }
}
```
W powyższym przykładzie sprawdzamy, czy katalog "C:/Users/Ja/Programy" istnieje. Jeśli chcesz sprawdzić inny katalog, wystarczy zmienić ścieżkę w odpowiednim miejscu.

# Głębszy wgląd

Przyjrzyjmy się teraz niektórym ważniejszym aspektom sprawdzania, czy katalog istnieje w Javie.

- Metoda "exists" zwraca wartość typu boolean, co oznacza, że może przyjąć jedynie wartości "true" lub "false".
- Jeśli chcemy sprawdzić, czy dany katalog istnieje, możemy wykorzystać także metody "isDirectory" lub "isFile", które zwracają wartość "true" lub "false" w zależności od tego, czy dany plik jest katalogiem czy plikiem.
- W przypadku, gdy chcemy stworzyć nowy katalog o danej nazwie, warto najpierw sprawdzić, czy nie istnieje już katalog o tej samej nazwie, aby uniknąć konfliktów.
- W celu zachowania czytelności kodu, możemy wykorzystać także metodę "createDirectories", która pozwala na utworzenie katalogu wraz z całym drzewem podkatalogów, jeśli takie już istnieją.

# Zobacz także

- Dokumentacja Java dotycząca sprawdzania czy katalog istnieje: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html#exists(java.nio.file.Path) 
- Przydatne informacje o klasie "Files" w Javie: https://www.baeldung.com/java-nio-2-files 
- Poradnik na temat pracy z plikami i katalogami w Javie: https://javastart.pl/programowanie/java-nio-2-operacje-na-plikach