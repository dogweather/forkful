---
title:    "Java: Konwersja ciągu znaków na małe litery"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie danych na małe litery jest częstym zadaniem wykonywanym w programowaniu Java. Jest to ważne, ponieważ pozwala na porównywanie i analizowanie napisów w sposób jednolity i spójny. W tym artykule omówimy, dlaczego warto przetwarzać dane na małe litery i jak to zrobić w prosty sposób.

## Jak to zrobić

Aby przekonwertować napis na małe litery w Javie, wystarczy użyć metody `toLowerCase()`. Przykładowy kod wyglądałby następująco:

```Java
String napis = "PRZYKŁADOWY NAPIS";
String napisMaly = napis.toLowerCase();
System.out.println(napisMaly);
```

Powyższy przykład wyświetli na ekranie "przykładowy napis". Metoda `toLowerCase()` zwraca nowy obiekt typu String, więc warto przypisać go do nowej zmiennej.

Innym sposobem na konwersję napisu na małe litery jest użycie metody `toUpperCase()` w celu zamiany wszystkich liter na duże, a następnie metody `toLowerCase()` dla uzyskania małych liter.

```Java
String napis = "Przykładowy napis";
String napisMaly = napis.toUpperCase().toLowerCase();
System.out.println(napisMaly);
```

## Deep Dive

Podczas konwersji na małe litery należy pamiętać o tym, że zależy to od ustawień lokalnych systemu operacyjnego. Jeśli system operacyjny jest ustawiony na język, który używa znaków Unicode, wynikiem działania metody `toLowerCase()` może być inny niż oczekiwany. Aby uniknąć tego problemu, można użyć metody `toLowerCase(Locale.ROOT)`.

Kolejną ważną kwestią jest to, że metoda `toLowerCase()` działa tylko dla liter łacińskich. Jeśli potrzebujemy konwertować napis składający się z liter z alfabetów innych języków, należy użyć metody `toLowerCase(Locale)` i podać odpowiednie ustawienia regionalne. Na przykład, dla napisu w języku polskim, powinniśmy użyć `toLowerCase(new Locale("pl"))`.

## Zobacz także

- Java String methods: https://www.javatpoint.com/java-string
- Java Locale class: https://www.baeldung.com/java-locale
- Unicode character sets: https://unicode-table.com/en/