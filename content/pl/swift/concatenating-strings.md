---
title:    "Swift: Łączenie ciągów znaków"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Dlaczego String Concatenation jest ważna dla nauki Swift?

String concatenation, czyli łączenie ciągów znaków, jest ważną i podstawową umiejętnością dla każdego programisty Swift. Pozwala na tworzenie dynamicznych i interaktywnych wiadomości lub wyświetlanie różnych wariantów tekstu w zależności od zmiennej lub warunków. Jest to również kluczowy element w budowaniu interfejsów użytkownika i obsłudze danych w wielu aplikacjach. Dlatego warto poznać dobrze tę funkcję i nauczyć się jej wykorzystywać.

## Jak wykonać string concatenation w Swift

Aby połączyć dwa lub więcej ciągów znaków w Swift, możemy użyć operatora plus (+) lub metody append(). Przykładowo:

```Swift
let str1 = "Cześć"
let str2 = "Polacy"

// użycie operatora plus
let greeting = str1 + " " + str2

// użycie metody append()
var message = "Lubię "
message.append(str2)

print(greeting) // wyświetli "Cześć Polacy"
print(message) // wyświetli "Lubię Polacy"
```

Operator plus pozwala nam łączyć dowolną ilość ciągów znaków, podczas gdy metoda append() działa tylko dla zmiennych. Aby uniknąć błędów, ważne jest również pamiętanie o dodawaniu odpowiednich spacji między słowami, jak w przykładzie powyżej.

## Deep Dive: Wszystko o string concatenation w Swift

W Swift, string concatenation może być wykonana za pomocą operatora plus (+), operatora += oraz metody append(). Operatory te działają inaczej dla różnych typów zmiennych. Dla typu Int lub Double, operator plus pozwala na dodawanie liczb, a operator += dokonuje natomiast prostego dodawania i przypisania. Dla typu Bool, operator plus po prostu łączy ciągi znaków "true" i "false". Dlatego też, jeśli chcemy wykonać concatenation dla zmiennych różnych typów, powinniśmy użyć metody append().

Dodatkowo, w Swift istnieje również wiele metod, takich jak uppercased(), lowercased(), czy count, które pozwalają na modyfikację lub manipulację ciągami znaków. Dzięki nim możemy np. zmienić wielkość liter, zamienić poszczególne znaki, lub poznać liczbę znaków w ciągu.

# Zobacz też

- Dokumentacja Swift - [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Wideo tutorial - [https://www.youtube.com/watch?v=6A9i7LObJOg](https://www.youtube.com/watch?v=6A9i7LObJOg)
- Przewodnik po String Concatenation w Swift - [https://www.hackingwithswift.com/quick-start/understanding-swift/how-to-concatenate-strings](https://www.hackingwithswift.com/quick-start/understanding-swift/how-to-concatenate-strings)