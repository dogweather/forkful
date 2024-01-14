---
title:    "C++: Zapisywanie ciągu znaków wielkimi literami"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Niejednokrotnie przy tworzeniu aplikacji czy gier, chcemy aby wyświetlane teksty były czytelne dla użytkownika. Jednym ze sposobów na to jest poprawne wykorzystanie wielkich liter w tekście. W tym artykule opiszemy jak w prosty sposób przekształcić tekst tak, aby zawierał tylko wielkie litery.

## Jak To Zrobić

```C++
#include <iostream>
#include <string>
#include <cctype>

std::string capitalize(std::string str) {
  for (int i = 0; i < str.length(); i++) {
    str[i] = toupper(str[i]);
  }
  return str;
}

int main() {
  std::string text = "wyraz do kapitalizacji";
  std::cout << capitalize(text) << std::endl;
  // Output: WYRAZ DO KAPITALIZACJI
  return 0;
}
```

Kod powyżej wykorzystuje funkcję `toupper()` z biblioteki `cctype`, która zamienia pojedynczą literę na jej odpowiednik z wielką literą. Funkcja `capitalize()` iteruje po każdej literze w tekście i przekazuje ją do `toupper()`, a następnie zwraca przekształcony tekst.

## Deep Dive

Wielkość liter w tekście jest określana przez standard Unicode. W przypadku języka angielskiego, funkcja `toupper()` działa poprawnie i zamienia litery na ich odpowiedniki z wielką literą. Jednakże w innych językach, jak na przykład polski, funkcja ta może nie działać prawidłowo. W takim przypadku, zaleca się użycie funkcji `std::locale` w celu ustalenia poprawnego kodowania znaków.

## Zobacz również

- [Dokumentacja `toupper()` w języku C++](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [Dokumentacja klasy `std::locale`](https://en.cppreference.com/w/cpp/locale/locale)