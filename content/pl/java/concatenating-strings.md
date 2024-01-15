---
title:                "Łączenie ciągów znaków"
html_title:           "Java: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Otwórzmy oczy na prawdę - ktoś z nas nie kocha łączenia stringów? Oczywiście, nie jest to funkcja, która wzbudza zachwyt, ale jest niezbędna w wielu aspektach programowania. Omówimy więc, dlaczego warto poświęcić czas na naukę konkatenacji stringów w Javie.

## Jak to zrobić?

Istnieją różne sposoby na łączenie stringów w Javie, ale najpopularniejszym i najprostszym jest użycie operatora "+".

```Java
String firstName = "John";
String lastName = "Doe";

String fullName = firstName + " " + lastName;

System.out.println(fullName);
```

Output: "John Doe"

Możemy również użyć metody "concat()" dostępnej w klasie String.

```Java
String firstWord = "Hello";
String secondWord = "World";

String result = firstWord.concat(secondWord);

System.out.println(result);
```

Output: "HelloWorld"

## Deep Dive

Istnieją również inne metody w klasie String, które pozwalają na łączenie stringów. Jedną z nich jest metoda "join()", która pozwala na połączenie wielu stringów w jeden, przy użyciu określonego separatora.

```Java
String[] words = {"Hello", "my", "name", "is", "John"};

String result = String.join(" ", words);

System.out.println(result);
```

Output: "Hello my name is John"

Inną ciekawą funkcją jest klasa StringBuffer, która zapewnia lepszą wydajność łączenia wielu stringów w przypadku, gdy potrzebujemy często zmieniać zawartość.

```Java
StringBuffer sb = new StringBuffer();

sb.append("Hello");
sb.append(" World");

String result = sb.toString();

System.out.println(result);
```

Output: "Hello World"

Jedną z najważniejszych rzeczy, które należy pamiętać podczas łączenia stringów, jest to, że tworzą one nowy obiekt, a nie modyfikują istniejący. Dlatego też ważne jest, aby nie używać operatora "+" w pętli, ponieważ może to prowadzić do problemów z wydajnością.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o łączeniu stringów w Javie, polecamy zapoznać się z dokumentacją Javy oraz innymi artykułami na ten temat:

- [Official Java documentation for String class](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Concatenating Strings in Java](https://www.baeldung.com/java-concatenate-strings)
- [Effective Java - Item 63: Beware the performance of string concatenation](https://www.informit.com/articles/article.aspx?p=1216151)