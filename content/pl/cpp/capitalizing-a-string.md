---
title:                "C++: Zastosowanie wielkich liter w ciągu znaków"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego zainteresować się zmianą wielkości liter w C++?

Manipulacja ciągami znaków jest nieodłączną częścią programowania w języku C++. Czasami istnieje potrzeba zmiany wielkości liter w danym ciągu na zdefiniowaną przez nas formę. Może to być wyświetlanie tekstu w jednolitej postaci lub też poprawianie błędów użytkowników wprowadzających dane. W tym artykule przedstawię prosty sposób na zmianę wielkości liter w C++ oraz kilka ciekawostek na temat tego zagadnienia.

## W jaki sposób zmienić wielkość liter w C++?

Zmiana wielkości liter w C++ jest możliwa dzięki użyciu funkcji `toupper()` i `tolower()` z biblioteki `cctype`. Dzięki nim możemy zmienić każdy znak w ciągu znaków na jego odpowiednik w dużej lub małej literze. Poniżej przedstawiamy prosty kod w C++, który dokonuje tej zmiany:

```C++
#include <iostream>
#include <cctype>

using namespace std;

int main() {
    string tekst = "Pierwsze zdanie.";

    for (int i = 0; i < tekst.length(); i++) {
        tekst[i] = tolower(tekst[i]);
    }

    cout << tekst;

    return 0;
}

```

#### Output:

`pierwsze zdanie.`

## Głębsze wgląd w zmianę wielkości liter

W powyższym przykładzie wykorzystaliśmy funkcję `tolower()` do zmniejszenia wielkości liter, ale w przypadku zmiany na duże litery, będziemy musieli użyć funkcji `toupper()`. Warto również zauważyć, że funkcje te nie zmieniają znaków specjalnych, takich jak kropki czy przecinki. Oznacza to, że jeśli mamy ciąg znaków, w którym występują te znaki, musimy dokonać kolejnej operacji na stosownych indeksach.

Inną ciekawą funkcją jest `isupper()` i `islower()`, które pozwalają sprawdzić, czy dany znak jest literą dużej lub małej wielkości. Dzięki nim możemy zastosować określone warunki w naszym kodzie, aby dokonać zmiany tylko na wybranych znakach.

## Zobacz również

- [Dokumentacja funkcji toupper() i tolower() w C++](https://www.cplusplus.com/reference/cctype/toupper/)
- [Wartości ASCII znaków](http://www.asciitable.com/)

Dziękujemy za przeczytanie tego artykułu. Mam nadzieję, że teraz wiesz jak zmienić wielkość liter w C++. Jeśli masz jakieś dodatkowe pytania lub uwagi, śmiało możesz zostawić je w komentarzu poniżej. Do zobaczenia w kolejnych artykułach!