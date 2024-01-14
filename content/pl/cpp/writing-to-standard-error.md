---
title:    "C++: Pisanie do standardowego błędu"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego pisać do standardowego błędu?

Pisanie do standardowego błędu może nie być najpopularniejszym zagadnieniem w świecie programowania, ale jest bardzo ważne w procesie debugowania kodu. Korzystanie z tej funkcji pozwala programiście na wyświetlenie informacji o błędach, ostrzeżeń lub wyjątków, które wystąpiły w trakcie działania programu. Jest to nieocenione narzędzie do identyfikacji i naprawy problemów w kodzie.

## Jak to zrobić?

Aby móc pisać do standardowego błędu w języku C++, musimy najpierw zaimportować bibliotekę `iostream` za pomocą słowa kluczowego `#include`. Następnie możemy skorzystać z obiektu `cerr` (standardowy strumień błędów), który jest dostępny w tej bibliotece. W poniższym przykładzie wyświetlimy komunikat o błędzie wraz z wartością zmiennej `x`:

```C++
#include <iostream>

int main() {
    int x = 10;
    std::cerr << "Wystąpił błąd! Wartość x: " << x << std::endl;
    return 0;
}
```

Po uruchomieniu tego kodu, zobaczymy taki wynik w konsoli:

```
Wystąpił błąd! Wartość x: 10
```

Możemy również skorzystać z obiektu `clog` (standardowy strumień logów), który służy do wyświetlania informacji o przebiegu programu. W poniższym przykładzie wyświetlimy komunikat o powitaniu użytkownika:

```C++
#include <iostream>

int main() {
    std::clog << "Witaj użytkowniku!" << std::endl;
    return 0;
}
```

Output:

```
Witaj użytkowniku!
```

## Głębsze zagadnienia

Pisanie do standardowego błędu nie ogranicza się tylko do wyświetlania prostych komunikatów. Możemy wykorzystać tę funkcję do zapisu informacji o błędach do pliku, dzięki czemu będziemy mogli później szczegółowo przeanalizować problem. Możemy również manipulować formatowaniem wyświetlanych komunikatów za pomocą słowa kluczowego `std::setw()`.

## Zobacz także

- [Dokumentacja standardowej biblioteki C++](https://cppreference.com)
- [Tutorial o pisaniu do standardowego błędu](https://www.tutorialspoint.com/how-to-write-to-standard-error-in-cplusplus)
- [Przykłady użycia standardowego błędu w praktyce](https://thispointer.com/c-how-to-write-error-logs-in-a-separate-file/#files)