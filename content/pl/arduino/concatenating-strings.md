---
title:                "Arduino: Konkatenacja łańcuchów"
simple_title:         "Konkatenacja łańcuchów"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu z użyciem Arduino, łączenie lub łączenie ciągów jest częstym zadaniem. Jest to przydatne w celu łączenia różnych znaków lub wartości liczbowych w jeden ciąg, który można wykorzystać do wyświetlenia danych lub przekazania ich do innego urządzenia. W tym artykule dowiecie się, jak w prosty sposób wykonywać tę operację i jak możecie ją wykorzystać w swoich projektach.

## Jak to zrobić

Aby skonkatenować ciągi w Arduino, używamy funkcji `strcat()` lub operatora `+`. Na przykład:

```Arduino
 char ciag1[] = "Hello";
 char ciag2[] = "world!";
 
 strcat(ciag1, ciag2);
 
 Serial.println(ciag1);
```

W powyższym przykładzie używamy funkcji `strcat()` do połączenia dwóch ciągów w jeden. Połączyliśmy "Hello" i "world!" w "Hello world!" i wyświetliliśmy go za pomocą funkcji `Serial.println()`.

Możemy również użyć operatora `+` do skonkatenowania ciągów:

```Arduino
char ciag1[] = "Lubię";
char ciag2[] = "Arduino";

Serial.println(ciag1 + " " + ciag2);
```

W wyniku tego kodu otrzymamy ciąg "Lubię Arduino".

## Głębsza analiza

Podczas korzystania z funkcji `strcat()` lub operatora `+` należy pamiętać o kilku ważnych aspektach. Po pierwsze, musimy łączyć tylko ciągi tego samego typu, na przykład tylko `char`, a nie `char` i `int`. Po drugie, należy zapewnić wystarczającą ilość pamięci dla skonkatenowanego ciągu. Jeśli pamięci nie będzie wystarczająco, może dojść do błędu lub niepożądanego rezultatu.

## Zobacz także

- [Funkcja strcat() w dokumentacji Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/strcat/)
- [Operator + w dokumentacji Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/operator-plus/)
- [Przykładowe projekty z użyciem łączenia ciągów w Arduino](https://create.arduino.cc/projecthub/tags/string+concatenation)