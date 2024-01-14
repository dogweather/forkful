---
title:    "Arduino: Zamiana tekstu na dużą literę"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego?

Każdy programista wie, jak ważne jest prawidłowe formatowanie tekstu w aplikacjach. Często zdarza się, że konieczne jest zmienienie normalnego tekstu na duże litery, na przykład do wyświetlenia tytułu. W takiej sytuacji nie chcemy przepisywać całego tekstu ręcznie, ale skorzystać z prostego sposobu, którym jest zaprogramowanie kapitalizacji ciągu znaków. W tym artykule dowiesz się, jak to zrobić przy użyciu Arduino.

## Jak to zrobić?

Aby zaprogramować kapitalizację ciągu znaków w Arduino, będziemy potrzebować funkcji dostępnej w bibliotece "string" o nazwie "toUpperCase()". Sprawdźmy, jak wykorzystać tę funkcję w praktyce:

```Arduino
#include <string.h>

void setup() {
  Serial.begin(9600);
  // Tworzymy zmienną typu string z naszym tekstem
  String tekst = "to jest przykładowy tekst";
  
  // Wyświetlamy wartość zmiennej "tekst"
  Serial.println(tekst);
  
  // Korzystając z funkcji "toUpperCase()" zamieniamy wszystkie litery na duże
  String kapitalizowany_tekst = tekst.toUpperCase();
  
  // Wyświetlamy wartość zmiennej "kapitalizowany_tekst"
  Serial.println(kapitalizowany_tekst);
}

void loop() {
  // puste
}
```

Po wgraniu tego kodu na płytę Arduino i otwarciu monitora szeregowego, powinniśmy zobaczyć w nim nasz tekst wyświetlony dwukrotnie - raz w oryginalnej formie, a następnie w wersji z wszystkimi literami zamienionymi na duże.

## Głębszy zanurzenie

Funkcja "toUpperCase()" dostępna w bibliotece "string" działa na podstawie standardu ASCII. Oznacza to, że zamieniane są tylko litery z alfabetu łacińskiego, a cyfry, znaki specjalne czy litery z innych alfabetów pozostają bez zmian. 

Funkcja ta również działa na podstawie znaków Unicode, co oznacza, że może poprawnie kapitalizować litery z różnych alfabetów (na przykład greckiego czy hebrajskiego). 

Warto również pamiętać, że funkcja ta działa w sposób nie-destruktywny, czyli nie zmienia oryginalnego ciągu znaków, tylko tworzy nowy ciąg zawierający wartość po kapitalizacji.

## Zobacz również

- Dokumentacja biblioteki "string" z funkcją "toUpperCase()": https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- Strona z kodem Unicode: http://unicode.org/charts/pdfs/Unicode-9.0/U90-10A0.pdf
- Poradnik programowania w Arduino: https://wiki.arcbotics.com/index.php?title=Programming_an_Arduino&Language=pl