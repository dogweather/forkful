---
title:                "Java: Znajdowanie długości ciągu znaków"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, podczas pisania programów w Javie, musimy mieć możliwość obliczenia długości tekstu, który użytkownik wprowadzi do naszej aplikacji. Może być to przydatne w przypadku walidacji danych lub oddzielania słów w zdaniach. W tym artykule omówimy, jak obliczyć długość łańcucha znaków za pomocą Java.

## Jak to zrobić

Aby obliczyć długość łańcucha znaków, możemy użyć metody `length()` z klasy `String`. Przykładowy kod wyglądałby następująco:

```Java
String text = "To jest przykładowy tekst.";
int length = text.length();
System.out.println("Długość tekstu wynosi: " + length);
```

Powyższy kod utworzy zmienną `text`, która przechowuje nasz tekst, następnie wywoła metodę `length()` na tej zmiennej i przypisze wynik do zmiennej `length`. Na koniec wyświetli wynik w konsoli.

Możemy również obliczyć długość tekstu bez przypisywania jej do zmiennej, za pomocą dowolnej zmiennej typu `String`, np.:

```Java
System.out.println("Długość tekstu: " + "To jest inny tekst.".length());
```

Wynikiem tego kodu będzie wyświetlenie w konsoli "Długość tekstu: 17".

Możemy również sprawdzić długość tekstu wprowadzonego przez użytkownika podczas działania programu, na przykład:

```Java
Scanner scanner = new Scanner(System.in);
System.out.print("Wprowadź dowolny tekst: ");
String userInput = scanner.nextLine();
System.out.println("Długość tekstu: " + userInput.length());
```

W tym przypadku, program zapyta użytkownika o wprowadzenie tekstu, a następnie wyświetli jego długość.

## Pogłębione zagadnienia

Podczas obliczania długości tekstu warto zwrócić uwagę na kilka rzeczy. Po pierwsze, długość tekstu jest mierzona w liczbie znaków, nie w słowach. Oznacza to, że spacja jest również traktowana jako znak i jest wliczana w długość.

Po drugie, metoda `length()` nie może być wywoływana na wartościach null, w przeciwnym razie spowoduje to wystąpienie błędu.

Warto również zauważyć, że metoda `length()` jest często wykorzystywana do iterowania przez wszystkie znaki w tekście przy użyciu pętli for lub while.

## Zobacz również

* [Jak porównywać napisy w Java](https://www.samouczekprogramisty.pl/porownywanie-napisow-w-javie/)
* [Jak zwracać podciągi (substring) w Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
* [Metody klasy String w Javie](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)