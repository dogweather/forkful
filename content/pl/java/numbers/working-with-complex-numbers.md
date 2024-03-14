---
date: 2024-01-26 04:42:32.151190-07:00
description: "Liczby zespolone rozszerzaj\u0105 lini\u0119 liczb rzeczywistych przez\
  \ dodanie jednostki urojonej, `i`, gdzie `i^2 = -1`. S\u0105 one kluczowe w takich\
  \ dziedzinach jak\u2026"
lastmod: '2024-03-13T22:44:35.271224-06:00'
model: gpt-4-0125-preview
summary: "Liczby zespolone rozszerzaj\u0105 lini\u0119 liczb rzeczywistych przez dodanie\
  \ jednostki urojonej, `i`, gdzie `i^2 = -1`. S\u0105 one kluczowe w takich dziedzinach\
  \ jak\u2026"
title: Praca z liczbami zespolonymi
---

{{< edit_this_page >}}

## Co i dlaczego?

Liczby zespolone rozszerzają linię liczb rzeczywistych przez dodanie jednostki urojonej, `i`, gdzie `i^2 = -1`. Są one kluczowe w takich dziedzinach jak inżynieria, fizyka i zaawansowana matematyka, gdzie modelują zjawiska, z którymi liczby rzeczywiste nie mogą sobie poradzić, takie jak prądy elektryczne i przetwarzanie sygnałów.

## Jak to zrobić:

Java nie posiada wbudowanego wsparcia dla liczb zespolonych, ale możemy stworzyć własną klasę lub użyć biblioteki. Oto krótki przykład, jak utworzyć prostą klasę `ComplexNumber` i użyć jej:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString do wyświetlania liczb zespolonych w formie a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Szybki test
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Suma: " + c1.add(c2));
    }
}
```

Przykładowe wyjście dla metody main będzie:

```
Suma: 3.0 + 7.0i
```

## Szczegółowa analiza

Zanim pojawiły się języki wysokiego poziomu takie jak Java, programiści pracowali bezpośrednio z bibliotekami matematycznymi w językach takich jak Fortran czy C, aby zarządzać złożonymi operacjami. Koncepcja ta sięga XVI wieku, za co są uznawani matematycy tacy jak Gerolamo Cardano i Rafael Bombelli.

W Javie, `java.lang.Math` jest miejscem, do którego się udaje po niezbędne elementy, ale pomija liczby zespolone, prawdopodobnie dlatego, że nie każdy programista ich używa. Alternatywy? Użyj bibliotek. Apache Commons Math zapewnia klasę `Complex` wypełnioną metodami do manipulacji. Oto dlaczego tworzenie własnej klasy jest jednak fajne: lekkość, dostosowanie do dokładnych potrzeb i brak nadmiaru biblioteki.

Jedna ważna rzecz: uważaj na precyzję liczby zmiennoprzecinkowej. Komputery nie mogą przedstawić niektórych liczb dokładnie, co prowadzi do błędów zaokrąglenia. Podczas wykonywania powtarzalnych złożonych operacji te błędy mogą się kumulować!

## Zobacz również

Aby zgłębić temat i bardziej złożone operacje, sprawdź:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [Klasa Complex w JScience](http://jscience.org/)
- Poradniki Oracel'a na temat [arytmetyki zmiennoprzecinkowej](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
