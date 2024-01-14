---
title:    "Kotlin: Wydrukowanie wyjścia debugowania"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Wpis debugowania to ważna umiejętność każdego programisty. Wyświetlanie informacji o przebiegu działania programu jest niezwykle pomocne w identyfikacji błędów i usprawnianiu kodu. W tym artykule dowiesz się, dlaczego warto wyświetlać wyniki debugowe w języku Kotlin.

## Jak to zrobić

Aby wyświetlić informacje debugowe w Kotlinie, możemy skorzystać z funkcji `println()` lub `debugPrint()` z biblioteki standardowej. Możemy również wykorzystać odpowiednie tagi w `Log` w przypadku pisania kodu dla systemu Android. Poniżej znajdują się przykładowe kody wraz z wypisanymi wynikami.

```Kotlin
val number = 10
val name = "John"

println(number) // wypisanie wartości zmiennej number
println("Cześć, $name") // wypisanie tekstu z wykorzystaniem zmiennej name

// wyniki:
// 10
// Cześć, John 

debugPrint("Liczba: $number") // wypisanie wartości ze specjalnym prefiksem
debugPrint("Hello, $name")

// wyniki:
// D/DEBUG_PRINT: Liczba: 10
// D/DEBUG_PRINT: Hello, John 
```
W powyższych przykładach widzimy, że możemy wyświetlać zarówno same wartości zmiennych, jak i łączyć je z tekstem.

## Dogłębnie

Warto pamiętać, że wyświetlanie informacji debugowych nie jest tylko pomocne w przypadku naprawiania błędów. Może również być wykorzystywane do analizy działania programu i lepszego zrozumienia jego przebiegu. Dodatkowo, wyświetlanie tesktu może pomóc w testowaniu różnych funkcji i warunków w kodzie.

Należy jednak pamiętać, aby usuwać lub zakomentować wyświetlane informacje debugowe w wersji finalnej programu. Niepotrzebne wyświetlanie wielu informacji może spowolnić działanie programu lub utrudnić jego czytelność.

## Zobacz również

* [Dokumentacja funkcji println w języku Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/println.html)
* [Przewodnik po debugowaniu w języku Kotlin](https://www.kotlindevelopment.com/kotlin-debug-logging/)
* [Wykorzystanie Logcat w procesie debugowania aplikacji na system Android](https://developer.android.com/studio/debug/am-logcat.html)