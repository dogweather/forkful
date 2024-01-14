---
title:    "Ruby: Zmiana wielkości litery w ciągu znaków"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedykolwiek chcieć zmienić wygląd wyrazu lub zdania na wielkie litery? Może dla estetyki lub do zastosowań praktycznych? W tym artykule dowiesz się, dlaczego i jakie narzędzia Ruby dostarcza do zmiany wielkości liter w napisach.

## Jak to zrobić

Istnieje kilka sposobów, aby zmienić wielkość liter w napisach w Ruby. Jedną z najprostszych metod jest użycie metody `upcase`, która zamienia wszystkie litery w ciągu na duże litery. Przykład kodu:

```Ruby
napis = "witaj świecie!"
puts napis.upcase
```

Output: WITAJ ŚWIECIE!

Inną metodą jest używanie metody `capitalize`, która zmienia pierwszą literę wyrazu na dużą, a pozostałe na małe. Przykład kodu:

```Ruby
napis = "hello world"
puts napis.capitalize
```

Output: Hello world

Aby zmienić tylko pierwszą literę całego zdania, można użyć metody `capitalize`, a następnie dodać pozostałe litery wyrazów za pomocą metody `downcase`. Przykład kodu:

```Ruby
napis = "witaj świecie!"
puts napis.capitalize + napis.downcase[1..-1]
```

Output: Witaj świecie!

## Głębsze zanurzenie

Metody `upcase` i `capitalize` są tylko kilkoma z wielu dostępnych metod do zmiany wielkości liter w Ruby. Istnieją również metody `swapcase`, która zamienia duże litery na małe i odwrotnie, oraz `titleize`, która zamienia pierwszą literę każdego wyrazu na dużą. Polecam przetestować wszystkie metody i wybrać najodpowiedniejszą w danym przypadku.

## Zobacz również

- [Dokumentacja Ruby o zmianie wielkości liter](https://ruby-doc.org/core-2.7.2/String.html#method-i-upcase)
- [Tutorial na stronie Medium o zmianie wielkości liter w Ruby](https://medium.com/@jazimecki/change-case-in-ruby-string-helpers-badc9c0a1d3a)
- [Wideo ze szkolenia Ruby dla początkujących, które również omawia zmianę wielkości liter](https://www.youtube.com/watch?v=n_Ofc5sbmCg)