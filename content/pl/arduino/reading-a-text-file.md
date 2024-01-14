---
title:    "Arduino: Czytanie pliku tekstowego"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego warto czytać pliki tekstowe?

Czytanie plików tekstowych jest częstym zadaniem w programowaniu Arduino. Dzięki temu możesz wczytać dane z różnych źródeł, takich jak sensory lub urządzenia podłączone do Arduino. W ten sposób masz dostęp do informacji, które mogą pomóc w podejmowaniu decyzji lub sterowaniu innymi elementami Twojego projektu.

# Jak to zrobić?

Aby czytać pliki tekstowe w Arduino, możesz skorzystać z funkcji "File" i "FileReader". Najpierw musisz utworzyć obiekt "File" i podać nazwę pliku, który chcesz otworzyć. Następnie można utworzyć obiekt "FileReader" z użyciem obiektu "File" jako parametru. W tym momencie plik jest już otwarty i możesz użyć funkcji "readLine()", aby odczytać kolejne linie tekstu.

```Arduino
// Utworzenie obiektu "File"
File plik;

// Przypisanie nazwy pliku do obiektu "File"
plik = SD.open("dane.txt");

// Utworzenie obiektu "FileReader" z użyciem obiektu "File"
FileReader czytnik(plik);

// Odczyt kolejnych linii tekstu z pliku
String linia = czytnik.readLine();
```

# Pogłębiona analiza

Istnieje wiele innych funkcji, które mogą być przydatne podczas czytania plików tekstowych w Arduino. Na przykład, funkcja "available()" zwraca ilość dostępnych bajtów do odczytania, a "read()" pozwala na odczyt pojedynczego znaku z pliku. Możliwości są niemal nieograniczone i warto eksperymentować z różnymi funkcjami, aby dostosować je do swoich potrzeb.

# Zobacz również

Jeśli chcesz dowiedzieć się więcej o czytaniu plików tekstowych w Arduino, możesz zajrzeć na poniższe strony:

- [Dokumentacja Arduino dotycząca funkcji "File" i "FileReader"](https://www.arduino.cc/reference/en/libraries/sd-card-library/file/)
- [Przykładowe projekty wykorzystujące czytanie plików tekstowych](https://www.arduinolab.net/arduino-sd-cards.php)
- [Poradnik wideo na temat czytania i zapisywania danych z pliku na karcie SD z użyciem Arduino](https://www.youtube.com/watch?v=ml2Mw1IVYis)