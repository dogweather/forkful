---
title:    "Java: Łączenie ciągów znaków"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu często będziesz musiał manipulować ze stringami. Jednym z najważniejszych sposobów na to jest wzajemne łączenie czyli konkatenacja. W tym artykule dowiesz się po co i jak prosto to zrobić w języku Java.

## Jak to zrobić

Kodowanie przykładów i oczekiwany wynik zostaną przedstawione w blokach kodu "```Java ... ```". Język Java zapewnia nam prosty sposób na łączenie stringów przy użyciu operatora "+" lub metody "concat()". Oto kilka przykładowych kodów:

```Java
String imie = "Anna";
String nazwisko = "Nowak";
System.out.println(imie + " " + nazwisko); // wyświetli "Anna Nowak"
String pelneImie = imie.concat(" ").concat(nazwisko);
System.out.println(pelneImie); // wyświetli "Anna Nowak"
```

## Głębsza analiza

Operacja konkatenacji polega na łączeniu dwóch bądź więcej stringów w jeden. W języku Java jest to bardzo proste dzięki dostępnym operatorom i metodzie. Warto zauważyć, że operacja ta tworzy nowy string, a nie modyfikuje oryginalnych danych. Warto również zadbać o odpowiednie formatowanie podczas konkatenacji, aby uniknąć błędów.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o konkatenacji stringów w języku Java, zerknij na poniższe linki:

- [Java String Class](https://www.w3schools.com/java/java_string.asp)
- [Concatenation and Formatting in Java](https://www.baeldung.com/java-string-concatenation-formatting)
- [String Concatenation in Java](https://www.geeksforgeeks.org/concatenation-in-java/)