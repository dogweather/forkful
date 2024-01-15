---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Java: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, podczas pisania kodu w języku Java, może zdarzyć się, że będziemy musieli usunąć pewne znaki z naszych zmiennych lub stringów. Może to być konieczne, na przykład, do usunięcia znaków niedrukowalnych lub zastąpienia niechcianych znaków innymi.

## Jak To Zrobić

Usunięcie znaków pasujących do określonego wzorca w języku Java jest możliwe za pomocą metody `replaceAll()` i wyrażenia regularnego. Przykład kodu poniżej pokazuje jak usunąć wszystkie znaki spoza przedziału od A do Z i od 0 do 9 z ciągu znaków:

```Java
String sentence = "T3st0wy t3xt z z@naki #sp3?cjalne.";
String cleanSentence = sentence.replaceAll("[^A-Za-z0-9 ]", "");
System.out.println(cleanSentence);
```

Output: `T3st0wy t3xt z znaki spcjalne`

Jak widzimy, metoda `replaceAll()` zastępuje wszystkie znaki pasujące do podanego wzorca (w tym przypadku wszystkie znaki spoza przedziału od A do Z i od 0 do 9) pustym ciągiem znaków, czyli usuwa je.

Możemy również określić inne wzorce, np. aby usunąć wszystkie cyfry z tekstu, możemy użyć wyrażenia regularnego `[0-9]`, lub aby pozostawić tylko litery i spacje, możemy użyć `[A-Za-z ]`.

## Głębsze Zagłębianie Się

Podczas korzystania z metody `replaceAll()` ważne jest, aby pamiętać, że przyjmuje ona wyrażenie regularne jako pierwszy argument, a nie pojedynczy znak. Oznacza to, że jeśli chcemy usunąć wszystkie wystąpienia konkretnej litery lub znaku, musimy użyć znaku `\\` przed tym znakiem w wyrażeniu regularnym. Na przykład, aby usunąć wszystkie wystąpienia litery "a" z tekstu, musimy użyć `sentence.replaceAll("a", "")`, ale aby usunąć wszystkie znaki "a", musimy użyć `sentence.replaceAll("\\\\", "")`.

Dodatkowo, metoda `replaceAll()` jest wrażliwa na wielkość liter, więc trzeba uważać na to, jakie znaki i litery są zawarte w wyrażeniu regularnym. Jeśli chcemy, aby metoda zignorowała wielkość liter, musimy dodać `(?i)` na początku wyrażenia regularnego. Na przykład, aby usunąć wszystkie litery "a" niezależnie od wielkości, możemy użyć `sentence.replaceAll("(?i)a", "")`.

## Zobacz również

- [Java - Strings and Regular Expressions](https://www.programiz.com/java-programming/strings-regular-expressions)
- [Regular Expressions in Java](https://www.baeldung.com/java-regular-expressions)
- [RegExr - Online tool for testing regular expressions](https://regexr.com/)