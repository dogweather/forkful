---
title:    "C++: Porównywanie dwóch dat"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

### Dlaczego

W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego życia. Musimy pisać kody na co dzień, aby rozwiązywać różne problemy i tworzyć innowacyjne rozwiązania. Jedną z podstawowych umiejętności w programowaniu jest porównywanie różnych wartości, w tym także dat. W tym artykule dowiecie się, dlaczego jest to ważne oraz jak wykonać porównywanie dwóch dat w języku C++.

### Jak to zrobić

Porównywanie dwóch dat w języku C++ może wydawać się trudne na początku, ale jest to w rzeczywistości dość proste. Do porównania dat będziemy używać struktury `tm` oraz funkcji `difftime()`. Poniżej przedstawione jest przykładowe porównanie dwóch dat i jego wynik w postaci kodu w języku C++:

```
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  struct tm firstDate, secondDate;
  double difference;

  firstDate.tm_year = 2020 - 1900;
  firstDate.tm_mon = 6 - 1;
  firstDate.tm_mday = 8;

  secondDate.tm_year = 2020 - 1900;
  secondDate.tm_mon = 8 - 1;
  secondDate.tm_mday = 20;

  difference = difftime(mktime(&secondDate), mktime(&firstDate));

  if (difference > 0) {
    cout << "Druga data jest późniejsza od pierwszej.";
  } else if (difference < 0) {
    cout << "Pierwsza data jest późniejsza od drugiej.";
  } else {
    cout << "Daty są identyczne.";
  }

  return 0;
}
```

Przykład ten wykorzystuje funkcję `mktime()` do konwersji daty z struktury `tm` do wartości liczbowej, a następnie funkcję `difftime()` do obliczenia różnicy pomiędzy datami. Następnie używamy instrukcji warunkowych `if...else` do porównania wyniku i wyświetlenia odpowiedniej informacji.

### Rzeczowa analiza

Mimo że porównanie dwóch dat jest pozornie prostym zadaniem, może ono stwarzać pewne problemy i wymagać głębszej analizy. W języku C++, daty są przechowywane jako wartości liczbowe, które są konwertowane przy użyciu różnych formatów. Dlatego ważne jest, aby upewnić się, że porównujemy daty w tym samym formacie i uwzględniamy różne ustawienia regionalne. Ważne jest również, aby uwzględnić przestępne lata i różnice w długości miesięcy.

### Zobacz także

- [Dokumentacja języka C++ na temat funkcji difftime()](https://en.cppreference.com/w/cpp/chrono/c/difftime)
- [Porównywanie dat w języku C++ - poradnik na stronie programiz.com](https://www.programiz.com/cpp-programming/library-function/ctime/difftime)