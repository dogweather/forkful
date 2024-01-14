---
title:    "Swift: Konwertowanie stringa na małe litery"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu niezbędne jest zmianie tekstu na małe litery. Przykładowo, może to być potrzebne podczas walidacji danych lub przy porównywaniu ciągów znaków. W tym artykule dowiesz się, jak w prosty sposób przekonwertować string na małe litery w języku Swift.

## Jak to zrobić

Aby przekonwertować string na małe litery w Swift, wystarczy użyć metody `lowercased()`. Przykładowo:

```Swift
let sentence = "CZEŚĆ ŚWIECIE!"
let lowercaseSentence = sentence.lowercased()
print(lowercaseSentence)
```
Output:
`cześć świat!`

Jak widać, metoda `lowercased()` przekonwertowała wszystkie litery w stringu na małe, co jest bardzo przydatne w różnych sytuacjach.

## Głębszy wgląd

Istnieje kilka ważnych rzeczy, o które warto wiedzieć odnośnie konwersji stringów na małe litery w Swift. Po pierwsze, należy zwrócić uwagę na to, że metoda `lowercased()` zwraca nową wartość, a nie modyfikuje oryginalnego stringa. Oznacza to, że musimy przypisać wartość zwróconą przez tę metodę do nowej zmiennej.

Kolejną ciekawostką jest to, że metoda `lowercased()` nie tylko przekonwertuje duże litery na małe, ale także odpowiada za konwersję znaków specjalnych, jak np. polskie litery. Możesz to sprawdzić, wykonując następujący kod:

```Swift
let sentence = "Żółw"
let lowercaseSentence = sentence.lowercased()
print(lowercaseSentence)
```
Output:
`żółw`

Warto również wiedzieć, że metoda `lowercased()` przyjmuje opcjonalny parametr `locale`, który pozwala na ustawienie odpowiednich reguł dla konwersji zgodnie z danym językiem.

## Zobacz również

- [Dokumentacja Apple - lowercased()](https://developer.apple.com/documentation/foundation/nsstring/1412469-lowercased)
- [Tutorial wideo od Ray Wenderlich - Konwersja Stringów w Swift](https://www.youtube.com/watch?v=9_8qOr91SCw)
- [Blog Swift Developer - Dlaczego warto przekonwertować string na małe litery?](https://swiftdeveloper.blog/konwersja-stringow-szkil-programowania/)