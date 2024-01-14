---
title:    "Java: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Przeszukiwanie i zamiana tekstu jest nieodłączną częścią procesu programowania w Javie. Jest to szczególnie przydatne, gdy musimy dokonać wielu zmian w dużej ilości tekstu. W tym artykule dowiesz się, dlaczego warto poznać tę umiejętność oraz jak dokonać skutecznej zamiany tekstu.

## Jak to zrobić

Aby przeszukiwać i zamieniać tekst w Javie, musimy skorzystać z metody replace() klasy String. Przykładowe użycie tej metody wygląda następująco:

```Java
String text = "Witaj, świecie!";
String nowyTekst = text.replace("świecie", "Javo");
System.out.println(nowyTekst);
```

Powinniśmy otrzymać odpowiedź: "Witaj, Javo!" Metoda replace() zwraca nowy String z dokonanymi zmianami, ale nie zmienia oryginalnego tekstu.

Aby dokonać zamiany tekstu w całym dokumencie, możemy wykorzystać pętlę for i metodę replace().

```Java
for(int i = 0; i < dokument.length; i++){
   dokument[i] = dokument[i].replace("slowo", "nowe slowo");
}
```

W ten sposób dopasujemy i zamienimy wszystkie wystąpienia słowa w dokumencie.

## Deep Dive

String w Javie jest niemodyfikowalny, dlatego metoda replace() zwraca nowy String ze zmienioną wartością. Jeśli zastosujemy ten sam String do wielu zmiennych, każda zmiana spowoduje utworzenie nowego obiektu String. Dlatego zaleca się stosowanie StringBuffer lub StringBuilder do przeprowadzania zmian w tekście, ponieważ są one efektywniejsze w porównaniu do String.

Inną metodą przeszukiwania i zamiany tekstu w Javie jest wykorzystanie wyrażeń regularnych. Dzięki nim możemy wyszukać określone wzorce i zamienić je na inne wyrażenie. Przykładowe użycie:

```Java
String text = "To jest przykładowy numer telefonu: 123-456-789";
String nowyTekst = text.replaceAll("\\d{3}-\\d{3}-\\d{3}", "XXX-XXX-XXX");
System.out.println(nowyTekst);
```
Powinniśmy otrzymać odpowiedź "To jest przykładowy numer telefonu: XXX-XXX-XXX".

## Zobacz także

- [Java String API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Porównanie pomiędzy StringBuffer a StringBuilder w Javie](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)
- [Wyrażenia regularne w Javie](https://regexr.com/)