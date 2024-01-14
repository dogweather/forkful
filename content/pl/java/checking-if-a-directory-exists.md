---
title:                "Java: Sprawdzanie czy istnieje katalog"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzenie, czy dany katalog istnieje, jest często niezbędnym krokiem podczas pisania programów Java. Może to być przydatne, gdy aplikacja musi sprawdzić, czy istnieje odpowiedni katalog do zapisu danych lub wczytania plików. W tym artykule dowiesz się, jak to zrobić w prosty sposób.

## Jak Wykonać

Aby sprawdzić, czy katalog istnieje w programie Java, należy użyć metody `exists()` z klasy `java.io.File`. Przykładowy kod wygląda następująco:
```
Java File directory = new File("sciezka/do/katalogu");
if(directory.exists()){
    System.out.println("Katalog istnieje.");
} else{
    System.out.println("Katalog nie istnieje.");
}
```
Jeśli katalog istnieje, na ekranie zostanie wyświetlona informacja "Katalog istnieje.". W przeciwnym razie zostanie wyświetlona informacja "Katalog nie istnieje.". W ten sposób możemy łatwo sprawdzić, czy katalog istnieje i wykonać odpowiednie akcje w zależności od wyniku.

## Dogłębna Analiza

Metoda `exists()` zwraca wartość logiczną, czyli `true` lub `false`. Jeśli katalog istnieje, zostanie zwrócona wartość `true`, w przeciwnym razie zostanie zwrócona wartość `false`. Metoda ta nie jest w stanie rozróżnić, czy podana ścieżka jest katalogiem czy plikiem. Jeśli chcesz sprawdzić, czy podana ścieżka jest katalogiem, możesz użyć metody `isDirectory()`, która zwraca również wartość logiczną.

## Zobacz również
- [Dokumentacja Java - metoda exists()](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#exists())
- [Dokumentacja Java - metoda isDirectory()](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#isDirectory())
- [Tutorial Java - Praca z plikami](https://www.javatpoint.com/java-file)

Dziękujemy za przeczytanie tego artykułu na temat sprawdzania, czy katalog istnieje w języku Java. Mam nadzieję, że był on dla Ciebie pomocny i pozwolił łatwiej rozwiązać Twoje problemy związane z pracą z plikami. Bądź na bieżąco z naszymi kolejnymi poradnikami dotyczącymi programowania w języku Java!