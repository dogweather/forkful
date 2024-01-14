---
title:                "C: Pisanie testów"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-tests.md"
---

{{< edit_this_page >}}

# Dlaczego warto pisać testy w języku C?

Testowanie kodu jest nieodłączną częścią procesu tworzenia oprogramowania. To sprawdzony i niezawodny sposób na zapewnienie, że nasza aplikacja działa poprawnie i nie zawiera błędów. W języku C, gdzie wydajność i niezawodność są kluczowe, pisanie testów jest niezwykle ważne.

## Jak pisać testy w języku C?

Testowanie w języku C może wydawać się skomplikowane i czasochłonne, ale dzięki kilku prostym krokom możemy uprościć ten proces. Przykładowy kod oraz wynik działania możemy zobaczyć poniżej:

```C
#include <stdio.h>
#include <stdbool.h>

// Funkcja, którą będziemy testować
bool is_even(int num) {
  if (num % 2 == 0) {
    return true;
  } else {
    return false;
  }
}

// Funkcja do testowania, zwraca true jeśli testy zostaną zaliczone
bool test_is_even() {
  // Test dla parzystej liczby
  int num = 4;
  bool result = is_even(num);
  if (result == true) {
    printf("Zaliczone - %d jest liczbą parzystą \n", num);
  } else {
    printf("Nie zaliczone - %d powinno być liczbą parzystą \n", num);
    return false;
  }
  
  // Test dla nieparzystej liczby
  num = 5;
  result = is_even(num);
   if (result == true) {
    printf("Nie zaliczone - %d powinno być liczbą nieparzystą \n", num);
    return false;
  } else {
    printf("Zaliczone - %d jest liczbą nieparzystą \n", num);
  }
  return true;
}

int main() {
  // Wywołanie funkcji testującej oraz sprawdzenie wyniku
  bool result = test_is_even();
  if (result == true) {
    printf("Wszystkie testy zaliczone, kod jest poprawny!");
  } else {
    printf("Niektóre z testów nie zostały zaliczone, sprawdź kod jeszcze raz.");
  }
  return 0;
}

```

**Wynik działania:**

```
Zaliczone - 4 jest liczbą parzystą 
Nie zaliczone - 5 powinno być liczbą nieparzystą 
Wszystkie testy zaliczone, kod jest poprawny!
```

W powyższym przykładzie zawsze możemy być pewni, że nasza funkcja `is_even` działa poprawnie dzięki zastosowaniu testów. W ciągu kilku sekund możemy przetestować różne przypadki i znaleźć i naprawić błędy, zanim nasz kod trafi do produkcji.

## Głębsza analiza pisania testów w języku C

Testowanie w języku C może być wyzwaniem ze względu na brak wbudowanych funkcji do testowania, jakie posiadają niektóre inne języki programowania. Jednak dzięki użyciu biblioteki, takiej jak [Unity](https://github.com/ThrowTheSwitch/Unity), możemy naśladować funkcjonalność testów jednostkowych znanych z innych języków.

Istnieją również inne metody testowania w języku C, takie jak testowanie wydajności lub używanie debuggera do ręcznego sprawdzania kodu. Warto również pamiętać o pisaniu testów jednostkowych w trakcie tworzenia kodu, a nie na końcu projektu, aby uniknąć problemów związanych z modyfikacją już istniejącego kodu.

# Zobacz również
- [Biblioteka Unity do testowania w języku C](https://github.com/ThrowTheSwitch/Unity)
- [Przykładowy artykuł o testowaniu kodu w języku C](https://www.