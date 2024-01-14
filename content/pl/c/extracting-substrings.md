---
title:    "C: Wycinanie podciągów"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego

Dlaczego warto nauczyć się wyciągać podciągi w programowaniu? Przede wszystkim pozwala to na efektywne wykorzystanie danych i znacznie ułatwia operacje na napisach. W tym blogu dowiesz się, jak to zrobić w języku C!

# Jak to zrobić

Pierwszym krokiem jest użycie funkcji `substring()` w celu wydobycia wybranej części napisu. Przykładowo, jeśli chcemy wyodrębnić pierwsze trzy litery z napisu "Hej wszystkim!", używamy kodu:

```C
char* str = "Hej wszystkim!";
char* sub = substring(str, 0, 3);
printf("%s", sub);
```

Wynikiem będzie napis "Hej". Możemy także wyodrębnić podciąg od wybranego indeksu do końca napisu, wystarczy w drugim parametrze `substring()` wpisać wartość `-1`, np:

```C
char* str = "Hej wszystkim!";
char* sub = substring(str, 4, -1);
printf("%s", sub);
```

W takim przypadku wynikiem będzie "wszystkim!". Należy jednak pamiętać, że należy wziąć pod uwagę liczbę indeksu oraz znaki nowej linii i zakończenia napisu `\0`.


# Głębsze zanurzenie

Mimo że wyciąganie podciągów jest stosunkowo proste, warto poznać kilka istotnych elementów związanych z tą operacją. W przypadku gdy podciąg ma być wydobyty od pierwszego indeksu, nie musimy określać parametru `start` w funkcji `substring()`, możemy po prostu zastosować następujący zapis:

```C
char* str = "Hej wszystkim!";
char* sub = substring(str, 0, -1);
printf("%s", sub);
```

Używając funkcji `substring()` możemy także przypisać wydobyty podciąg bezpośrednio do zmiennych typu `char*`, np:

```C
char* str = "Hej wszystkim!";
char* sub1, sub2, sub3;
substring(str, 0, 3, &sub1);
substring(str, 4, -1, &sub2);
substring(str, 8, -1, &sub3);
```

W ten sposób będziemy mieć dostęp do poszczególnych podciągów poprzez zmienne `sub1`, `sub2` i `sub3`.

# Zobacz także

- Dokumentacja języka C: https://en.cppreference.com/w/c
- Tutoriale związane z programowaniem w C: https://www.tutorialspoint.com/cprogramming/index.htm 
- Rozwiązania zadań z podciągów w języku C: https://www.geeksforgeeks.org/substring-subset-strings-c/