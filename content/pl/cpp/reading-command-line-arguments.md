---
title:    "C++: Odczytywanie argumentów z wiersza poleceń"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w C++ to wciągająca i potrzebna umiejętność, a dzięki umiejętności czytania argumentów wiersza poleceń możemy zbudować jeszcze lepsze i bardziej funkcjonalne programy. Dlatego, czytając ten blog post, dowiesz się jak odczytywać argumenty wiersza poleceń w C++.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń w C++, musimy użyć klasy "argc" i "argv". Oto przykład kodu:

```C++
#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {

  // pętla odczytująca argumenty
  for(int i = 0; i < argc; i++) {
    cout << "Argument " << i << " to: " << argv[i] << endl;
  }

  return 0;
}
```

Przykładowe wywołanie programu: `./myprogram argument1 argument2`. Otrzymasz następujący wynik:

```
Argument 0 to: ./myprogram
Argument 1 to: argument1
Argument 2 to: argument2
```

W powyższym kodzie, `argc` zawiera liczbę argumentów, a `argv` jest tablicą zawierającą same argumenty. Dzięki temu możemy przetwarzać i wykorzystywać te argumenty w naszym programie.

## Głębsze zagłębianie

Czytanie argumentów wiersza poleceń może być przydatne w wielu przypadkach. Możemy na przykład przekazywać wartości lub opcje do naszego programu, w zależności od tego, jak zawołamy go z wiersza poleceń. Możemy również wykorzystać argumenty do sterowania tym, co nasz program ma zrobić.

Ważne jest, aby pamiętać, że pierwszym argumentem w `argv` jest nazwa naszego programu, a każdy kolejny jest kolejnym argumentem. Możemy wykorzystać tę wiedzę do dokładniejszego przetwarzania argumentów i ich wykorzystania w naszym programie.

## Zobacz również

- [Dokumentacja C++ o użyciu argumentów wiersza poleceń](https://www.cplusplus.com/articles/DEN36T05/)
- [Wideo na YouTube o czytaniu argumentów wiersza poleceń w C++](https://www.youtube.com/watch?v=63tGfP-qkRg)
- [Przykładowy kod na GitHubie wykorzystujący argumenty wiersza poleceń](https://github.com/username/myprogram/blob/master/main.cpp)