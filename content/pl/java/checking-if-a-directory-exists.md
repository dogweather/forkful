---
title:                "Sprawdzanie, czy istnieje katalog"
html_title:           "Java: Sprawdzanie, czy istnieje katalog"
simple_title:         "Sprawdzanie, czy istnieje katalog"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Przyjrzenie się, czy katalog istnieje, jest ważnym aspektem programowania w Javie, ponieważ pomaga upewnić się, że nasza aplikacja działa zgodnie z oczekiwaniami i nie generuje błędów.

## Jak To Zrobić

Sprawdzenie, czy dany katalog istnieje, można dokonać za pomocą klasy `File` i jej metody `exists()`. Poniżej znajduje się przykładowy kod, który pozwala na sprawdzenie istnienia katalogu "mojekatalogi":

```java
import java.io.File;

public class DirectoryExistsExample {

    public static void main(String[] args) {
        // utworzenie obiektu reprezentującego katalog "mojekatalogi"
        File directory = new File("mojekatalogi");
        
        // sprawdzenie, czy katalog istnieje
        if (directory.exists()) {
            System.out.println("Katalog istnieje!");
        } else {
            System.out.println("Katalog nie istnieje!");
        }
    }
}
```

W powyższym przykładzie używamy metody `exists()`, która zwraca wartość logiczną `true` lub `false` w zależności od istnienia danego pliku lub katalogu. W celu sprawdzenia istnienia katalogu, musimy utworzyć obiekt klasy `File` z nazwą katalogu, który chcemy sprawdzić.

Możemy również użyć metody `isDirectory()`, która pozwala sprawdzić, czy dany obiekt `File` reprezentuje katalog. Poniżej znajduje się zmodyfikowany przykład z wykorzystaniem tej metody:

```java
import java.io.File;

public class DirectoryExistsExample {

    public static void main(String[] args) {
        // utworzenie obiektu reprezentującego katalog "mojekatalogi"
        File directory = new File("mojekatalogi");
        
        // sprawdzenie, czy obiekt jest katalogiem
        if (directory.isDirectory()) {
            System.out.println("To jest katalog!");
        } else {
            System.out.println("To nie jest katalog!");
        }
    }
}
```

W obu przypadkach, jeśli jesteśmy pewni, że katalog powinien istnieć, ale nasz kod wciąż zwraca wartość `false`, może to oznaczać, że podana nazwa katalogu jest niepoprawna lub nie istnieje w przestrzeni dyskowej naszego systemu.

## Deep Dive

Metoda `exists()` sprawdza jedynie, czy dany plik lub katalog istnieje w danej chwili, nie analizując jego zawartości lub struktury. W celu bardziej szczegółowego sprawdzenia, czy dany katalog rzeczywiście zawiera pliki lub katalogi, możemy użyć metody `listFiles()`, która zwraca listę obiektów typu `File` dla wszystkich elementów znajdujących się w danym katalogu.

Przykładowo, jeśli chcemy sprawdzić, czy katalog "mojekatalogi" zawiera jakieś pliki lub podkatalogi, możemy zmodyfikować nasz drugi przykład w następujący sposób:

```java
import java.io.File;

public class DirectoryExistsExample {

    public static void main(String[] args) {
        // utworzenie obiektu reprezentującego katalog "mojekatalogi"
        File directory = new File("mojekatalogi");
        
        // utworzenie tablicy dla zawartości katalogu
        File[] files = directory.listFiles();
        
        // sprawdzenie, czy tablica nie jest pusta
        if (files != null) {
            if (files.length > 0) {
                System.out.println("Katalog zawiera " + files.length + " elementów!");
            } else {
                System.out.println("Katalog jest pusty!");
            }
        } else {
            System.out.println("Wystąpił błąd podczas pobierania zawartości katalogu!");
        }
    }
}
```

Metoda `listFiles()` w powyż