---
title:    "Java: Odczytywanie argumentów wiersza poleceń"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się chcieć wprowadzić pewne zmienne lub opcje do swojego programu po uruchomieniu go? Może chcesz umożliwić użytkownikowi wybór języka w swojej aplikacji lub dostosować zachowanie programu do warunków środowiska, w którym jest uruchamiana? To właśnie wtedy przydają się argumenty z wiersza poleceń. Czytanie tych argumentów może być bardzo pomocne, aby dostosować program do aktualnych potrzeb użytkownika lub maszyny, na której jest uruchomiony.

## Jak To Zrobić

Aby odczytać argumenty z wiersza poleceń w języku Java, możemy skorzystać z klasy `java.lang.String[] args`, która reprezentuje tablicę ciągów znaków (argumentów). Przykładowo, jeśli chcemy przekazać odpowiedzi do quizu podczas uruchamiania programu, możemy użyć poniższego kodu:

```Java
public class Quiz {
    public static void main(String[] args) {
        // pobranie argumentów z wiersza poleceń
        String response1 = args[0]; // odpowiada argumentowi podanemu jako pierwszy
        String response2 = args[1]; // odpowiada argumentowi podanemu jako drugi

        // wyświetlenie dostarczonych odpowiedzi
        System.out.println("Twoja pierwsza odpowiedź to: " + response1);
        System.out.println("Twoja druga odpowiedź to: " + response2);
    }
}
```

Jeśli uruchomimy powyższy program z podanymi argumentami `tak` oraz `nie`, otrzymamy następujący wynik:

```bash
$ java Quiz tak nie
Twoja pierwsza odpowiedź to: tak
Twoja druga odpowiedź to: nie
```

## Głębokie Nurkowanie

W powyższym przykładzie używaliśmy tylko dwóch argumentów, ale jesteśmy w stanie przekazać dowolną liczbę argumentów. Aby odczytać ich wartości, możemy skorzystać z pętli `for-each`, jak w poniższym przykładzie:

```Java
// pobranie i wyświetlenie wszystkich argumentów
for (String arg : args) {
    System.out.println("Kolejny argument to: " + arg);
}
```

Możemy również przekazywać argumenty w formie flag lub opcji, używając znaku `-` lub `--` przed nazwą argumentu, które następnie możemy odczytać i odpowiednie obsłużyć w naszym programie.

## Zobacz również

- Dokumentacja klasy `java.lang.String[] args`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Przykłady użycia argumentów z wiersza poleceń w języku Java: https://www.javatpoint.com/command-line-arguments-in-java
- Poradnik odczytu argumentów z wiersza poleceń w języku Java: https://www.baeldung.com/java-command-line-arguments