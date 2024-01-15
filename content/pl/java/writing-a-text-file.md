---
title:                "Tworzenie pliku tekstowego"
html_title:           "Java: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Pisanie plików tekstowych jest często niezbędnym elementem programowania w języku Java. Pozwala ono na zapisywanie i odczytywanie danych z plików, co jest przydatne w wielu aplikacjach. Jest to także ważny krok w nauce programowania.

## Jak to zrobić?

Aby zapisać tekst do pliku, należy użyć klasy `FileWriter` oraz `BufferedWriter`. Najpierw należy utworzyć obiekt klasy `FileWriter` z adresem pliku jako parametrem. Następnie należy utworzyć obiekt klasy `BufferedWriter`, który będzie zawierał metody do zapisu tekstu. Wreszcie, używając metody `write()` należy przekazać tekst, który chcemy zapisać do pliku. Przykładowy kod wygląda następująco:

```Java
FileWriter writer = new FileWriter("sciezka/do/pliku.txt");
BufferedWriter bWriter = new BufferedWriter(writer);
bWriter.write("To jest przykładowy tekst do zapisania.");
bWriter.close();
```

Jeśli chcemy odczytać tekst z pliku, należy wykorzystać klasę `FileReader` oraz `BufferedReader`. W tym przypadku, najpierw musimy otworzyć plik przy użyciu obiektu `FileReader`, a następnie przekazać go do obiektu `BufferedReader`. Aby odczytać tekst, możemy użyć metody `readLine()`, która odczytuje jeden wiersz tekstu z pliku. Przykładowy kod może wyglądać tak:

```Java
FileReader reader = new FileReader("sciezka/do/pliku.txt");
BufferedReader bReader = new BufferedReader(reader);
String line = bReader.readLine();
System.out.println(line); // wypisze: To jest przykładowy tekst do zapisania.
bReader.close();
```

## Przeanalizujmy dokładniej

Podczas pisania plików tekstowych, warto pamiętać o kilku ważnych zagadnieniach. Po pierwsze, należy uważać na używany encoding. Domyślnie, pliki zapisywane są z użyciem encodingu systemowego, ale można to zmienić w konstruktorze klasy `FileWriter` lub `FileReader`.
Kolejną ważną kwestią jest prawidłowe zamykanie strumieni, w naszych przykładach zastosowaliśmy to przy użyciu metody `close()`. Można także użyć bloku `try-finally`, w celu zapewnienia, że nie pozostaną otwarte strumienie nawet w przypadku wystąpienia błędu.

## Zobacz także

1. [Oficjalna dokumentacja Javy](https://docs.oracle.com/javase/10/)
2. [Poradnik dla początkujących w programowaniu w Javie](https://javastart.pl/kurs/java-podstawy/)
3. [Tutorial na temat operacji na plikach w Javie](https://www.baeldung.com/java-write-to-file)