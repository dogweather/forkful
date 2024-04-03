---
date: 2024-01-27 20:34:07.947459-07:00
description: "Generowanie losowych liczb polega na tworzeniu nieprzewidywalnych sekwencji\
  \ lub pojedynczych warto\u015Bci w okre\u015Blonym zakresie. Programi\u015Bci u\u017C\
  ywaj\u0105 tej\u2026"
lastmod: '2024-03-13T22:44:35.273264-06:00'
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb polega na tworzeniu nieprzewidywalnych sekwencji\
  \ lub pojedynczych warto\u015Bci w okre\u015Blonym zakresie."
title: Generowanie liczb losowych
weight: 12
---

## Co i dlaczego?

Generowanie losowych liczb polega na tworzeniu nieprzewidywalnych sekwencji lub pojedynczych wartości w określonym zakresie. Programiści używają tej techniki z różnych powodów, w tym do symulacji, gier, aplikacji bezpieczeństwa oraz metod próbkowania do testowania algorytmów w różnych warunkach.

## Jak to zrobić:

W Javie, generowanie losowych liczb można osiągnąć za pomocą klasy `Random` z pakietu `java.util`, lub klas `ThreadLocalRandom` i `SecureRandom` dla specyficznych przypadków użycia. Poniższe przykłady ilustrują, jak używać tych klas.

### Korzystanie z klasy `Random`
Klasa `Random` oferuje sposób na generowanie prostych pseudo-losowych liczb.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Tworzy obiekt Random

        int randInt = rand.nextInt(50); // Generuje losową liczbę całkowitą od 0 do 49
        double randDouble = rand.nextDouble(); // Generuje losową liczbę zmiennoprzecinkową między 0.0 a 1.0
        boolean randBoolean = rand.nextBoolean(); // Generuje losową wartość boolean
        
        System.out.println("Losowa Liczba Całkowita: " + randInt);
        System.out.println("Losowa Liczba Zmiennoprzecinkowa: " + randDouble);
        System.out.println("Losowa Wartość Boolean: " + randBoolean);
    }
}
```

### Korzystanie z klasy `ThreadLocalRandom`
Dla aplikacji współbieżnych, `ThreadLocalRandom` jest bardziej wydajna niż `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // Od 1 do 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // Od 1.0 do 10.0
        
        System.out.println("Losowa Liczba Całkowita: " + randInt);
        System.out.println("Losowa Liczba Zmiennoprzecinkowa: " + randDouble);
    }
}
```

### Korzystanie z klasy `SecureRandom`
Dla operacji kryptograficznych, `SecureRandom` zapewnia wyższy poziom bezpieczeństwa.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Wypełnia bajty bezpiecznymi losowymi liczbami
        
        System.out.println("Bezpieczne Losowe Bajty:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Pogłębiona analiza

Generowanie losowych liczb znacząco ewoluowało od wczesnych dni komputeryzacji. Klasa `Random` w Javie używa liniowej formuły kongruencyjnej do generowania pseudo-losowych liczb, które są deterministyczne i nie nadają się do aplikacji o wysokim poziomie bezpieczeństwa. To doprowadziło do wprowadzenia `SecureRandom`, które używa bardziej zaawansowanych algorytmów (np. SHA1PRNG) do produkcji kryptograficznie silnych losowych liczb.

Jednakże, `Random` i `SecureRandom` mają swoje wady, takie jak degradacja wydajności w środowiskach wielowątkowych. Klasa `ThreadLocalRandom` została wprowadzona w Javie 7, aby rozwiązać ten problem, oferując generator losowych liczb specyficzny dla wątku, znacząco poprawiający wydajność w aplikacjach współbieżnych.

Chociaż te klasy pokrywają większość potrzeb, dla bardzo dużych skali lub specjalistycznych wymagań, programiści mogą eksplorować dodatkowe biblioteki lub opracowywać własne rozwiązania. Istotne jest, aby wybrać odpowiednie podejście w oparciu o potrzeby bezpieczeństwa i wymagania wydajnościowe przypadku użycia.
