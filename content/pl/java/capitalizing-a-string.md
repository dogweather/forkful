---
title:                "Java: Zawężenie ciągu znaków"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach nauka programowania jest coraz ważniejsza. Wielu z nas zaczyna od podstawowych informacji na temat języka programowania Java, takich jak zmienne, pętle czy instrukcje warunkowe. Jednak wraz z rozwojem naszych umiejętności, chcemy wykorzystać inne funkcjonalności i narzędzia, aby nasze programy były jeszcze lepsze. Jedną z takich funkcjonalności jest zmiana wielkości liter w ciągu znaków, a dzisiaj dowiemy się, dlaczego warto się z nią zapoznać.

## Jak to zrobić
```Java
// Tworzymy ciąg znaków, który będziemy chcieli zmienić
String text = "programowanie jest super!";

// Wyświetlamy oryginalny ciąg znaków
System.out.println(text);

// Używamy metody toUpperCase() do zmiany wszystkich liter na wielkie
String capitalizedText = text.toUpperCase();

// Wyświetlamy zmieniony ciąg znaków
System.out.println(capitalizedText);
```
**Wyjście:**
```
programowanie jest super!
PROGRAMOWANIE JEST SUPER!
```

W powyższym przykładzie użyliśmy metody `toUpperCase()` dostępnej w klasie String, która pozwala nam zmienić wszystkie litery w danym ciągu znaków na wielkie. W ten sposób możemy w prosty sposób poprawić estetykę naszych napisów czy też zadbać o ich spójność.

## Głębsze zanurzenie

Metoda `toUpperCase()` jest tylko jednym z wielu sposobów na zmianę wielkości liter w ciągu znaków w języku Java. Istnieją również metody takie jak `toLowerCase()`, które zmienia wszystkie litery na małe, czy też `replace()`, która pozwala na zamianę konkretnych znaków w ciągu na inne. Istnieje także możliwość wykorzystania klasy `StringBuilder`, która daje nam większą kontrolę nad manipulacją ciągami znaków. Warto eksperymentować z różnymi rozwiązaniami i dostosować je do naszych potrzeb.

## Zobacz również

- Dokumentacja: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html
- Poradnik: https://www.javatpoint.com/java-string-replace
- Przydatne narzędzia: https://www.tutorialspoint.com/online_java_utilities/index.htm