---
title:    "Swift: Wydrukuj dane diagnostyczne"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami, podczas pisania aplikacji w Swift, trudno jest nam zrozumieć, co właściwie dzieje się w naszym kodzie. Być może otrzymujemy nieoczekiwane wyniki, bądź aplikacja po prostu nie działa tak, jak tego chcemy. W takich momentach przydatne może okazać się wypisywanie wiadomości debugowania, aby dowiedzieć się, co dokładnie dzieje się w naszym kodzie. W tym artykule dowiesz się, dlaczego warto używać wypisywania debugowania i jak tego dokonać w języku Swift.

## Jak To Zrobić

Aby wypisać wiadomości debugowania w Swift, użyjemy funkcji `print()`. Przykładowo, jeśli mamy zmienną `x` i chcemy sprawdzić jej wartość, możemy użyć następującego kodu:

``` Swift
let x = 5
print("Wartość zmiennej x to: \(x)")
```

Output tego kodu będzie wyglądał następująco:

```
Wartość zmiennej x to: 5
```

Możemy również wyświetlać więcej niż jedną zmienną w jednej linii, oddzielając je przecinkami, na przykład:

``` Swift
let a = "Hello"
let b = "World"
print("\(a), \(b)!")
```

Output:

```
Hello, World!
```

Dodatkowo, możemy również wypisywać wartości złożone, takie jak tablice czy słowniki. W przypadku tablic, możemy użyć metody `joined()` aby je połączyć, na przykład:

``` Swift
let fruits = ["apple", "banana", "orange"]
print("Moje ulubione owoce to: \(fruits.joined(separator: ", "))")
```

Output:

```
Moje ulubione owoce to: apple, banana, orange
```

## Głębszy Wgląd

Wypisywanie wiadomości debugowania może być bardzo przydatne podczas pisania aplikacji Swift. Jednak warto pamiętać, że częste używanie tej funkcji może spowolnić naszą aplikację. Dlatego powinniśmy używać jej tylko w trakcie testowania i debuggingu, a nie w finalnej wersji aplikacji.

Dodatkowo, funkcja `print()` może przybierać różne formy. Możemy na przykład wyświetlać debug output w konsoli, ale również zamieszczać go w plikach tekstowych czy zapisywać w dzienniku zdarzeń. To, w jaki sposób wypisujemy wiadomości, zależy od naszych potrzeb i preferencji.

## Zobacz również

- [10 sposobów na debugowanie kodu w Swift](https://medium.com/swlh/10-ways-to-debug-code-in-swift-847769cf2e35)
- [Oficjalna dokumentacja funkcji `print()` w Swift](https://developer.apple.com/documentation/swift/1541293-print)
- [Poradnik: Debugowanie aplikacji w Xcode](https://www.raywenderlich.com/7489882-ios-debugging-cheatsheet-for-xcode-12)