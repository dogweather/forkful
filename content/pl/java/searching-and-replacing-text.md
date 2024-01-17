---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Java: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

W programowaniu, "szukanie i zamienianie" tekstu (znane także jako "replace" lub "find and replace") oznacza wyszukiwanie określonego ciągu znaków w tekście i zamienianie go na inny ciąg. Programiści często stosują tę technikę, aby modyfikować wielokrotnie występujące wyrazy lub frazy w kodzie lub plikach tekstowych.

# Jak to zrobić:

### Przykłady kodu w Javie:

```Java
String text = "Cześć, nazywam się Jan i lubię programować.";
String newText = text.replace("Jan", "Kasia");
System.out.println(newText);
```

Wynik:
```
Cześć, nazywam się Kasia i lubię programować.
```

```Java
String program = "Java";
String newProgram = program.replace("a", "A");
System.out.println(newProgram);
```

Wynik:
```
JAvA
```

# Dogłębne zagłębienie:

### Kontekst historyczny:
Pierwsze wzmianki o funkcji "szukaj i zamień" można znaleźć w geodezji i kartografii w latach 40. XX wieku. Później została ona wprowadzona do programów tekstowych w latach 60. XX wieku, umożliwiając szybką i efektywną modyfikację tekstu.

### Alternatywy:
Istnieje wiele narzędzi i programów do znajdowania i zamieniania tekstu, w tym funkcje wbudowane w popularne edytory tekstu, takie jak Notepad++ czy Visual Studio Code.

### Szczegóły implementacyjne:
W języku Java, funkcja "replace" jest dostępna dla obiektów klasy String i umożliwia nie tylko zamianę pojedynczego znaku na inny, ale także zamianę całego wyrażenia. Można także określić, czy zamiana ma być wykonywana jedynie dla pierwszego wystąpienia lub dla wszystkich wystąpień w tekście.

# Zobacz także:

[Oficjalna dokumentacja Javy na temat funkcji replace](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)

[Inne funkcje dostępne dla obiektów String w Javie](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)