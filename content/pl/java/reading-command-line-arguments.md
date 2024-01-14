---
title:                "Java: Odczytywanie argumentów wiersza poleceń"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Dlaczego warto poznać odczytywanie argumentów wiersza poleceń?

W dzisiejszych czasach programiści muszą być wszechstronni i umieć obsługiwać różne narzędzia. Odczytywanie argumentów wiersza poleceń jest jedną z umiejętności, które mogą okazać się niezbędne w codziennej pracy. W tym artykule dowiesz się, dlaczego warto poznać tę funkcjonalność oraz jak ją wykorzystać w swoich projektach.

## Jak to zrobić?

Aby odczytać argumenty wiersza poleceń w języku Java, wystarczy użyć klasy `java.lang.String[] args`, która jest przekazywana jako parametr metody `main()`. Poniżej znajduje się przykładowy kod, który wyświetla argumenty wiersza poleceń w konsoli:

```java
public static void main(String[] args) {
    for(String arg : args) {
        System.out.println(arg);
    }
}
```

Przykładowy wynik dla wywołania `java Program arg1 arg2 arg3` będzie wyglądał następująco:

```
arg1
arg2
arg3
```

Zauważ, że argumenty są oddzielane spacją i są dostępne w postaci tablicy `String[]`. Oznacza to, że możesz przetwarzać je tak samo, jak inne tablice w Javie, na przykład za pomocą pętli `for` lub metod klasy `Arrays`.

## Glebokość

Odczytywanie argumentów wiersza poleceń może okazać się przydatne w wielu przypadkach. Na przykład, gdy tworzysz program, który wymaga podania danych wejściowych przez użytkownika lub gdy chcesz wywołać dany skrypt z różnymi parametrami. Dodatkowo, możesz przekazywać do programu flagi, które zmieniają jego działanie w zależności od wybranej opcji.

Ważne jest również zapewnienie poprawności wprowadzonych argumentów. Możesz na przykład sprawdzać, czy użytkownik podał odpowiednią ilość argumentów lub czy są one w odpowiednim formacie. W przypadku błędów, możesz wyświetlić użytkownikowi odpowiednie komunikaty lub przerwać działanie programu.

Innym ciekawym zastosowaniem jest tworzenie skryptów, które będą wykonywać określone zadania w zależności od podanych argumentów. Dzięki temu, możesz wykorzystać te same skrypty w różnych scenariuszach bez konieczności ich modyfikowania.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o odczytywaniu argumentów wiersza poleceń w Java, koniecznie zajrzyj na poniższe strony:

- Dokumentacja klasy `java.lang.String[] args` w języku Java: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html
- Przykładowe projekty wykorzystujące odczytywanie argumentów wiersza poleceń: https://www.codejava.net/java-se/command-line/parse-command-line-arguments-using-apache-commons-cli-library