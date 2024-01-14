---
title:    "Java: Zamiana na Wielkie Litery w Ciągu Tekstowym"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Dlaczego capitalizacja jest ważna w programowaniu Java?

Capitalizacja, czyli zamiana pierwszej litery w danym wyrażeniu na wielką, jest ważną częścią programowania. Wiele języków programowania, w tym Java, ma swoje własne konwencje dotyczące capitalizacji nazw zmiennych, metod i klas. Przestrzeganie tych konwencji nie tylko poprawia czytelność kodu, ale również ułatwia współpracę z innymi programistami. 

# Jak to zrobić w Java?

W Java, istnieje wbudowana funkcja o nazwie `toUpperCase()`, która może być użyta do zmiany pierwszej litery na wielką. Przykładowy kod wyglądałby następująco:

```Java
String name = "julia";
name = name.substring(0, 1).toUpperCase() + name.substring(1);

System.out.println(name); // Output: Julia
```

Funkcja `substring()` jest użyta, aby wydobyć pierwszą literę z wyrażenia i przekazać ją do funkcji `toUpperCase()`, która zmienia ją na wielką. Następnie, wyrażenie jest ponownie połączone z resztą wyrazu i przypisane do zmiennej `name`.

Można również użyć klasy `StringBuilder` do capatalizacji wyrażenia, co jest bardziej wydajnym sposobem w przypadku większych ciągów znaków. Przykładowy kod wyglądałby następująco:

```Java
String name = "adam";
StringBuilder sb = new StringBuilder(name);
sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));

System.out.println(sb.toString()); // Output: Adam
```

W tym przypadku, `StringBuilder` jest użyty do modyfikacji pierwszej litery w miejscu, co jest szybszym rozwiązaniem niż tworzenie nowego obiektu `String`.

# Głębsze wyjaśnienie

W języku Java, konwencją jest, aby pierwsza litera nazwy klasy była wielka, a pierwsze litery każdego wewnętrznego wyrazu w nazwie metody czy zmiennej także były duże, np. `MyClass`, `myMethod()`, `myVariable`. Dokładne zasady capitalizacji w Java można znaleźć w oficjalnej dokumentacji języka.

Należy również pamiętać, że function `toUpperCase()` zmieni tylko pierwszą literę w danym wyrażeniu, więc jeśli chcemy capitalizować całe wyrażenie, musimy najpierw użyć funkcji `toLowerCase()`, aby zmienić wszystkie litery na małe, a następnie zastosować `toUpperCase()`.

# Zobacz również

- [Oficjalna dokumentacja Java - Capitalization](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/variables.html)
- [Konwencje nazewnictwa w języku Java](https://www.geeksforgeeks.org/java-naming-conventions/)
- [Inne przydatne funkcje String w Java](https://www.programiz.com/java-programming/library/string)