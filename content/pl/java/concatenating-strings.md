---
title:    "Java: Łączenie ciągów znaków"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu w języku Java często spotykamy się z potrzebą łączenia (konkatenacji) łańcuchów znaków. Jest to nieodzowna część tworzenia skomplikowanych aplikacji, gdzie dane muszą być wyświetlane w czytelnej formie dla użytkownika. W tym artykule dowiesz się, dlaczego łączenie łańcuchów jest tak ważne i jak to zrobić w praktyce.

## Jak to zrobić

Java posiada wiele sposobów na konkatenację łańcuchów znaków. Najprostszym i najczęściej używanym jest operator "+", który pozwala łączyć dwa lub więcej łańcuchów w jeden. Przykładowe użycie tego operatora wygląda następująco:

```Java
String imie = "Anna";
String nazwisko = "Kowalska";
String pelneImie = imie + " " + nazwisko;
System.out.println(pelneImie);
```
Wynik działania powyższego kodu będzie wyglądał tak: "Anna Kowalska". Możemy również użyć metody `concat()`, która działa podobnie jak operator "+":

```Java
String imie = "Jan";
String nazwisko = "Nowak";
String pelneImie = imie.concat(" ").concat(nazwisko);
System.out.println(pelneImie);
```
Ten przykład również wyświetli na ekranie "Jan Nowak". Ważne jest, aby pamiętać o tym, że konkatenacja łańcuchów jest wykonywana od lewej do prawej strony, dlatego należy uważać na kolejność wyrażeń.

## Głębsza analiza

Podczas konkatenacji łańcuchów znaków, w rzeczywistości tworzona jest zupełnie nowa wartość, ponieważ łańcuchy są typem niemutowalnym w języku Java. Oznacza to, że po utworzeniu łańcucha, nie można go zmienić - każde jego modyfikacje będą skutkowały utworzeniem nowego obiektu. Dlatego też należy uważać na wydajność dla bardziej skomplikowanych operacji konkatenacji, ponieważ każda zmiana będzie generować nowy obiekt.

Warto również pamiętać, że konkatenacja nie ogranicza się tylko do łańcuchów znaków, ale może być użyta także do łączenia innych typów danych, np. liczb czy zmiennych typu boolean.

## Zobacz także

Jeśli jesteś zainteresowany/na pogłębienie swojej wiedzy na temat konkatenacji łańcuchów w Javie, polecam przeczytać:

- ["Concatenation" w Java documentation](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- ["String concatenation in Java" na GeeksForGeeks](https://www.geeksforgeeks.org/string-concatenation-in-java/)
- ["Performance of String Concatenation in Java" na Baeldung](https://www.baeldung.com/java-string-concatenation-performance)