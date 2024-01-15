---
title:                "Wyciąganie podciągów"
html_title:           "Java: Wyciąganie podciągów"
simple_title:         "Wyciąganie podciągów"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W artykule tym omówimy, dlaczego warto poznać technikę wycinania podciągów w języku Java. Będziemy prezentować przykłady kodu oraz przybliżymy informacje na temat tego zagadnienia.

## Jak to zrobić

Fragmentowanie stringów jest bardzo przydatną funkcją w języku Java, szczególnie przy pracy z tekstami. Pozwala ona na wyodrębnianie fragmentów tekstu, które są dla nas istotne, co ułatwia dalsze operacje. Poniżej prezentujemy kilka przykładów użycia z wyjaśnieniem.

```Java
// Wyciąganie podciągu od danego indeksu do końca tekstu
String sentence = "Programowanie w języku Java jest bardzo fascynujące.";
String substring = sentence.substring(30);
System.out.println(substring);
// Output: fascynujące.

// Wyciąganie podciągu od danego indeksu do innego podanego indeksu
String sentence = "Język Java jest bardzo popularny na całym świecie.";
String substring = sentence.substring(6, 10);
System.out.println(substring);
// Output: Java 
```

Powyższe przykłady pokazują, jak w prosty sposób wycinając odpowiednie fragmenty tekstu, możemy uzyskać dostęp do potrzebnych nam informacji. W przypadku wykorzystania tej funkcji w bardziej zaawansowanych projektach, z pewnością pomoże nam to w efektywnej i dokładnej obróbce danych.

## Głębszy zanurzenie

Metoda `substring` w języku Java jest często wykorzystywana przy pracy z indeksami. Warto mieć na uwadze, że numeracja indeksów rozpoczyna się od zera, co może być przydatne przy odwoływaniu się do konkretnych liter w tekście. Dodatkowo, istnieje również możliwość zastosowania metody `substring` do obiektów typu `StringBuilder`, co pozwala na edycję i modyfikację tekstu w miejscu.

Warto również zaznaczyć, że metoda ta posiada wiele różnych wariantów, co daje nam możliwość dostosowania jej do indywidualnych potrzeb. Na przykład, możemy podać indeksy w formie wyrażenia zamiast liczb, co zwiększa elastyczność jej użycia.

## Zobacz również

- [Dokumentacja Java - Manipulowanie stringami](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- [Jednostka zajęć - Wycinanie podciągów w języku Java](https://www.tutorialspoint.com/java/java_string_substring.htm)
- [Instrukcja Java - Metoda substring](https://www.geeksforgeeks.org/java-string-substring-method-example/)