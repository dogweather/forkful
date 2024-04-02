---
date: 2024-01-26 03:50:12.306170-07:00
description: "Oto ma\u0142a pr\xF3bka debugowania w Kotlinie z IntelliJ IDEA - Sherlocku\
  \ Holmesie w\u015Br\xF3d IDE: ```kotlin fun main() { val tajemniczaLiczba = 42 var\
  \ zgadnij = 0\u2026"
lastmod: '2024-03-13T22:44:35.369484-06:00'
model: gpt-4-0125-preview
summary: "Oto ma\u0142a pr\xF3bka debugowania w Kotlinie z IntelliJ IDEA - Sherlocku\
  \ Holmesie w\u015Br\xF3d IDE: ```kotlin fun main() { val tajemniczaLiczba = 42 var\
  \ zgadnij = 0\u2026"
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
Oto mała próbka debugowania w Kotlinie z IntelliJ IDEA - Sherlocku Holmesie wśród IDE:

```kotlin
fun main() {
    val tajemniczaLiczba = 42
    var zgadnij = 0

    while (zgadnij != tajemniczaLiczba) {
        println("Zgadnij liczbę: ")
        zgadnij = readLine()?.toIntOrNull() ?: continue // Ignoruj błędne dane wejściowe

        // Ustaw tutaj punkt przerwania, aby śledzić 'zgadnij' w akcji
        if (zgadnij < tajemniczaLiczba) {
            println("Za mało!")
        } else if (zgadnij > tajemniczaLiczba) {
            println("Za dużo!")
        }
    }

    println("Masz to! Tajemnicza liczba to $tajemniczaLiczba")
}
```

Wyjście debuggera:
```
Zgadnij liczbę: 
10
Za mało!
Zgadnij liczbę: 
50
Za dużo!
Zgadnij liczbę: 
42
Masz to! Tajemnicza liczba to 42
```

## Głębsze zanurzenie
Debuggery są w grze od lat '50. Wtedy były dość prymitywne, a debugowanie mogło dotyczyć bardziej sprzętu niż oprogramowania. Obecnie debugger taki jak w IntelliJ IDEA pozwala nam ustawiać punkty przerwania, krokować przez kod linijka po linijce i inspekcjonować stan zmiennych w naszym tempie.

Mimo że debugger IntelliJ jest bardzo przydatny dla Kotlina, to nie jest jedyną rybą w morzu. Jest wiele alternatyw, jak Logcat dla rozwoju aplikacji Android, czy narzędzia linii poleceń typu jdb dla minimalistów. Magia dziejąca się za kulisami to głównie o JVM Tool Interface (JVMTI), które pozwala debuggerom wchodzić w interakcję z maszyną wirtualną Javy, trzymając deweloperów Kotlina w pętli.

## Zobacz też
- Dokumentacja debuggera IntelliJ IDEA: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
