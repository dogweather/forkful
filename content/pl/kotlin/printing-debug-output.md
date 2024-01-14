---
title:    "Kotlin: Wyświetlanie informacji diagnostycznych"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista pewnie spotkał się z sytuacją, w której jego kod nie działał zgodnie z oczekiwaniami. W takiej sytuacji niezwykle pomocne okazują się wypisywanie informacji o wykonywanych operacjach, a także o wartościach zmiennych w poszczególnych krokach programu. Włączenie takiego debugowania może znacznie ułatwić nam znalezienie i naprawienie błędu. W tym artykule omówimy, jak użyć funkcji wypisywania do debugowania w języku Kotlin.

## Jak To Zrobić

Kotlin oferuje nam kilka sposobów na wypisywanie informacji w trakcie działania programu. Jednym z najprostszych jest użycie funkcji `println()`, która wypisuje podany argument na konsolę. Przykładowo, jeśli chcemy sprawdzić wartość zmiennej `counter` w pewnym punkcie naszego kodu, możemy wstawić poniższy fragment:

```Kotlin
println("Wartość counter to: $counter")
```

Jeśli zamiast samej wartości, chcemy wyświetlić też nazwę zmiennej, możemy skorzystać z tzw. string templates, czyli umieścić zmienną bezpośrednio wewnątrz tekstu:

```Kotlin
println("Wartość $counter to: $counter")
```

W ten sposób możemy wypisywać informacje o wielu zmiennych, a także formatować dane, np. wyświetlić je w odpowiedniej kolejności czy dodać opis.

Innym sposobem na wypisywanie debug output jest użycie funkcji `debug()` z biblioteki standardowej języka Kotlin. Musimy jednak najpierw zainstalować tę bibliotekę w naszym projekcie, dodając następującą linię do pliku `build.gradle`:

```Kotlin
dependences {
    implementation("org.jetbrains.kotlin:kotlin-stdlib:1.3.71")
}
```

Następnie w kodzie możemy użyć funkcji `debug()`, podając w niej treść naszej wiadomości. Jeśli chcemy wyświetlić więcej informacji, dodajemy je po przecinku:

```Kotlin
debug("Ustawiono wartość $value zmiennej $counter")
```

Możemy też wybrać, w jakim trybie ma być wyświetlana ta wiadomość - `debug()` jest dostosowany do trybów `true` i `false`. Domyślnie jest to tryb `false`, więc aby włączyć debugowanie, wystarczy zmienić wartość `debug` w naszych ustawieniach.

## Deep Dive

Wypisywanie informacji za pomocą funkcji `println()` nie jest jednak zalecane do większych projektów, ponieważ może wpłynąć na wydajność aplikacji. W takich przypadkach dobrze jest skorzystać z biblioteki `logcat`, która jest dostępna w większości środowisk programistycznych, takich jak IntelliJ IDEA czy Android Studio.

Aby korzystać z `logcat`, musimy najpierw zadeklarować zmienną wyjścia do logcat w jednym z naszych plików konfiguracyjnych. Następnie w kodzie możemy wypisywać informacje za pomocą funkcji `log()`:

```Kotlin
log("Wartość zmiennej $counter to: $counter")
```

`logcat` umożliwia także filtrowanie wyświetlanych informacji, przez co możemy wybrać, które wiadomości są dla nas istotne.

## Zobacz też

- [Dokumentacja Kotlin Logging](https://kotlinlang.org/docs/reference/logging.html)
- [Instrukcja wypisywania debug output w Android Studio](https://developer.android.com/studio/debug/am-logcat)