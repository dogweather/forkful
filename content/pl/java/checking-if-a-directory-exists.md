---
title:                "Java: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzenie, czy katalog istnieje, jest ważnym elementem w wielu projektach programistycznych. Dzięki temu możemy mieć pewność, że nasz kod będzie działał poprawnie i nie będzie wywoływał błędów związanych z brakiem wyników lub nieprawidłowymi ścieżkami. W tym artykule dowiesz się, jak w prosty sposób sprawdzić istnienie katalogu w języku Java.

## Jak to zrobić

Sprawdzenie istnienia katalogu w języku Java jest bardzo proste i może być wykonane za pomocą kilku linii kodu. Najpierw musimy utworzyć obiekt typu File, który będzie reprezentować nasz katalog. W tym celu użyjemy konstruktora, który jako argument przyjmie ścieżkę do naszego katalogu. Następnie wykorzystamy metodę exists() na tym obiekcie, która zwróci nam wartość logiczną true, jeśli katalog istnieje, lub false, jeśli nie istnieje. Przykładowy kod wyglądałby następująco:

```Java
File directory = new File("/sciezka/do/katalogu");
boolean exists = directory.exists();
```

Jeśli chcemy również wyświetlić informację o istnieniu katalogu, możemy użyć metody isDirectory(), która zwróci wartość true, jeśli nasz obiekt reprezentuje katalog, lub false w przeciwnym przypadku. W ten sposób możemy dodatkowo upewnić się, że sprawdzany przez nas element rzeczywiście jest katalogiem. Przykładowy kod z wykorzystaniem obu metod wyglądałby tak:

```Java
File directory = new File("/sciezka/do/katalogu");
boolean exists = directory.exists();
boolean isDirectory = directory.isDirectory();
System.out.println("Czy katalog istnieje? " + exists);
System.out.println("Czy to jest katalog? " + isDirectory);
```

W tym przypadku powinniśmy oczekiwać na wyjście "Czy katalog istnieje? true" oraz "Czy to jest katalog? true", jeśli nasz katalog istnieje i jest rzeczywiście katalogiem.

## Deep Dive

W przypadku, gdy chcemy bardziej szczegółowo przebadać nasz katalog, możemy również wykorzystać inne metody dostępne w klasie File. Na przykład, jeśli chcemy uzyskać listę plików znajdujących się wewnątrz naszego katalogu, możemy skorzystać z metody listFiles(), która zwraca tablicę obiektów File reprezentujących pliki. Możemy również sprawdzić, czy nasz katalog jest ukryty, wykorzystując metodę isHidden(). Wszystkie dostępne metody można znaleźć w dokumentacji języka Java.

## Zobacz także

- Java - dokumentacja klasy File: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Jak utworzyć nowy katalog w Java: https://javastart.pl/utworzenie-katalogu-folderu-w-javie/