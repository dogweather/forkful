---
date: 2024-01-26 01:40:14.416400-07:00
description: "Jak to zrobi\u0107: We\u017Amy prost\u0105 klas\u0119 Java, kt\xF3ra\
  \ wr\u0119cz wo\u0142a o refaktoryzacj\u0119 z powodu swojej z\u0142ej organizacji\
  \ i braku klarowno\u015Bci."
lastmod: '2024-03-13T22:44:35.285705-06:00'
model: gpt-4-0125-preview
summary: "We\u017Amy prost\u0105 klas\u0119 Java, kt\xF3ra wr\u0119cz wo\u0142a o\
  \ refaktoryzacj\u0119 z powodu swojej z\u0142ej organizacji i braku klarowno\u015B\
  ci."
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:
Weźmy prostą klasę Java, która wręcz woła o refaktoryzację z powodu swojej złej organizacji i braku klarowności.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Inne operacje...
    }
}
```

Po refaktoryzacji, mamy:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Inne operacje...
}
```

Dzięki refaktoryzacji poprawiliśmy nazwy metod i parametrów dla lepszej czytelności oraz usunęliśmy potrzebę istnienia warunkowego rozgałęzienia w jednej metodzie. Każda operacja teraz jasno określa swój cel.

## Pogłębiona wiedza:
Refaktoryzacja ma swoje korzenie w społeczności Smalltalk, z naciskiem na czytelność kodu i projektowanie zorientowane obiektowo, ale naprawdę nabrała tempa w świecie Java na przełomie lat 90. i 00., szczególnie po publikacji przełomowej książki Martina Fowlera "Refaktoryzacja: Ulepszanie struktury istniejącego kodu".

Istnieją alternatywy dla refaktoryzacji, takie jak pisanie kodu od nowa. Jednak refaktoryzacja jest często preferowana, ponieważ wiąże się z inkrementalnymi zmianami, które nie zakłócają funkcjonalności aplikacji.

Szczegóły implementacyjne przy refaktoryzacji w Java (lub w jakimkolwiek innym języku programowania) opierają się na zrozumieniu "zapachów kodu"—wskaźników głębszych problemów w kodzie. Do zapachów należą długie metody, duże klasy, zduplikowany kod oraz nadmierne użycie typów prymitywnych. Stosując wzorce refaktoryzacyjne, takie jak Ekstrakcja Metody, Przeniesienie Metody czy Zastąpienie Tymczasowej Zmienną Zapytaniem, programiści mogą systematycznie rozwiązywać te problemy, zapewniając jednocześnie, że kod pozostaje funkcjonalny przez cały czas.

Automatyczne narzędzia, takie jak wsparcie dla refaktoryzacji w IntelliJ IDEA, czy wtyczki do Eclipse, mogą pomoc w procesie, automatyzując refaktoryzację, taką jak zmienianie nazw zmiennych, metod i klas, ekstrahowanie metod lub zmiennych oraz przenoszenie metod lub klas do różnych pakietów lub przestrzeni nazw.

## Zobacz również:
- "Refaktoryzacja: Ulepszanie struktury istniejącego kodu" Martina Fowlera: https://martinfowler.com/books/refactoring.html
- Techniki refaktoryzacji na Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- Automatyczna refaktoryzacja w Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- Funkcje refaktoryzacji w IntelliJ IDEA: https://www.jetbrains.com/idea/features/refactoring.html

Każde z tych zasobów zapewnia podstawy do zrozumienia zasad refaktoryzacji lub narzędzia, które mogą być wykorzystane do wdrażania tych zasad w praktyce.
