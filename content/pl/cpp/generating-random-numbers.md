---
title:    "C++: Generowanie losowych liczb"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Dlaczego warto używać generowania losowych liczb w C++

Generowanie losowych liczb jest nieodzowną częścią programowania w wielu językach, w tym także w C++. Pozwala to na stworzenie bardziej dynamicznych i złożonych programów, przez co jest bardzo przydatne dla programistów.

# Jak to zrobić

Aby wygenerować losowe liczby w języku C++, należy skorzystać z funkcji `rand()` z biblioteki standardowej. Przykładowy kod będzie wyglądał następująco:

```C++
#include <iostream>
#include <stdlib.h>
using namespace std;

int main() {
    // Ustawienie ziarna dla generatora losowych liczb
    srand(time(0));

    // Wygenerowanie liczby pseudolosowej z zakresu od 0 do 99
    int liczba = rand() % 100;

    // Wyświetlenie wygenerowanej liczby
    cout << "Wygenerowana liczba to: " << liczba << endl;

    return 0;
}

```

Przykładowy output:

```
Wygenerowana liczba to: 74
```

Możemy również zastosować większą precyzję i ograniczyć zakres wygenerowanych liczb poprzez wykorzystanie dodatkowych funkcji matematycznych i arytmetycznych.

# Głębszy zanurzenie

Generowanie losowych liczb może być przydatne w różnych zastosowaniach, takich jak symulacje, tworzenie gier, generowanie kluczy kryptograficznych czy testowanie programów na warunkach brzegowych. W języku C++ generacja liczb jest oparta o algorytm nazywany "Mersenne Twister", który pozwala na uzyskanie bardzo dużego zakresu możliwych wygenerowanych liczb.

# Zobacz również

- [Dokumentacja biblioteki standardowej C++ - rand()](https://en.cppreference.com/w/cpp/numeric/random/rand)
- [Tutorial o generowaniu losowych liczb w C++](https://www.cplusplus.com/reference/random/)
- [Wykorzystanie funkcji `rand()` w tworzeniu gier w C++](https://www.gamedev.net/reference/articles/article248.asp)