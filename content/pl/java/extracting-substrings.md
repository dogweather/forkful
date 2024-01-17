---
title:                "Wydobywanie podciągów"
html_title:           "Java: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyodrębnianie podciągów jest procesem, w którym z danego ciągu znaków wyciągamy określony fragment. Programiści często wykonują tę operację, aby odfiltrować lub przekształcić dane w celu uzyskania potrzebnych informacji.

## Jak to zrobić:

```Java
// Przykład 1:
String str = "Hello World";
String subStr = str.substring(6);
System.out.println(subStr); // Output: World

// Przykład 2:
String str = "Lorem ipsum dolor sit amet";
int startIndex = 6;
int endIndex = 15;
String subStr = str.substring(startIndex, endIndex);
System.out.println(subStr); // Output: ipsum dol

```

## Głębsze zagłębienie

1. Kontekst historyczny:
Wyodrębnianie podciągów jest jedną z podstawowych operacji w programowaniu. Już w pierwszych językach programowania, takich jak Assembly czy Fortran, istniały funkcje umożliwiające przetwarzanie ciągów znaków.

2. Alternatywy:
Istnieje wiele alternatywnych sposobów wyodrębniania podciągów, takich jak używanie pętli lub zastosowanie metody split(). Jednak funkcja substring() jest najprostszym sposobem w Javie.

3. Szczegóły implementacji:
W Javie funkcja substring() używa indeksów, aby określić początkowy i końcowy punkt wyodrębnienia podciągu. Jest to ważne, aby pamiętać, że indeksy te są liczane od zera.

## Zobacz także:

- Dokumentacja Java String: https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/String.html#substring(int,int)
- Przykłady zastosowań substring(): https://www.geeksforgeeks.org/string-substring-method-in-java/