---
title:                "Java: Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, programiści często muszą zmagać się z dużymi i skomplikowanymi zbiorami danych. Często zdarza się, że te dane zawierają niepotrzebne znaki lub wzorce, które muszą zostać usunięte. Dzięki temu wpisowi dowiesz się, jak w łatwy sposób usunąć znaki, które odpowiadają określonemu wzorcowi, aby uprościć swoją pracę z danymi.

## Jak to zrobić

Do usuwania znaków dopasowanych do wzorca w języku Java, wykorzystać można metodę `replaceAll()` z klasy `String`.

Przykładowy kod wyglądałby następująco:

```Java
String str = "Ala ma kota, a kot ma Ale.";
String regex = "[aA]";
String replacedString = str.replaceAll(regex, ""); //usuwa wszystkie znaki 'a' i 'A'
System.out.println(replacedString); //Output: l m kot,  kot m le.
```

W powyższym przykładzie, wykorzystujemy metodę `replaceAll()`, aby zastąpić wszystkie wystąpienia znaków `a` i `A` pustym ciągiem znaków, tym samym usuwając je.

Można także wykorzystać wyrażenia regularne, aby usunąć określone wzorce z tekstu. Na przykład, jeśli chcemy pozostawić tylko cyfry w danym tekście, można użyć wyrażenia regularnego `[\\d]`, które oznacza "dowolna cyfra".

```Java
String str = "23Styczeń2020";
String regex = "[^\\d]";
String replacedString = str.replaceAll(regex, ""); //usuwa wszystkie znaki, które nie są cyframi
System.out.println(replacedString); //Output: 23012020
```

## Głębsza analiza

Metoda `replaceAll()` korzysta z wyrażeń regularnych, które są narzędziem do manipulacji tekstem, pozwalającym na wyrażanie wzorców poszukiwanych w tekście.

Należy pamiętać, że wyrażenia regularne są bardzo potężnym narzędziem, ale również mogą być bardzo skomplikowane. Właściwa wiedza i praktyka w ich wykorzystaniu jest kluczem do skutecznej pracy z tekstem.

## Zobacz także

- [Oficjalna dokumentacja Java - wyrażenia regularne](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Poradnik wyrażeń regularnych w Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)