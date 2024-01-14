---
title:    "Kotlin: Wydobywanie podciągów"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, programowanie jest nieodłączną częścią naszego życia. Jednym z elementów, które są niezbędne w tworzeniu aplikacji czy stron internetowych, jest manipulacja tekstami. Często zdarza się, że potrzebujemy wyodrębnić z danego tekstu określony fragment, zwany podłańcuchem. W tym artykule dowiesz się, dlaczego wyodrębnianie podłańcuchów jest tak ważne i jak to zrobić za pomocą języka Kotlin.

## Jak To Zrobić

Wyodrębnienie podłańcucha z tekstu w języku Kotlin jest bardzo proste. Najpierw musisz stworzyć zmienną przechowującą tekst, z którego chcesz wyodrębnić podłańcuch. Następnie, przy użyciu metody `.substring()` przekazujemy jej dwa argumenty: indeks początkowy oraz indeks końcowy podłańcucha.

```Kotlin
val tekst = "To jest przykładowy tekst do wyodrębnienia podłańcucha."
val podlancuch = tekst.substring(8, 21)

println(podlancuch) // wydrukuje "przykładowy tekst"
```

W powyższym przykładzie, podłańcuch został wyodrębniony od 8. do 21. znaku tekstu, włączając pierwszy i wyłączając ostatni znak. Natomiast jeśli chcemy wyodrębnić podłańcuch od konkretnego indeksu do końca tekstu, możemy pominąć drugi argument metody `.substring()`.

```Kotlin
val tekst = "To jest kolejny przykładowy tekst do wyodrębnienia podłańcucha."
val podlancuch = tekst.substring(8)

println(podlancuch) // wydrukuje "kolejny przykładowy tekst do wyodrębnienia podłańcucha."
```

Możemy również wykorzystać indeksy ujemne, które liczą się od końca tekstu. Dzięki temu, możemy wyodrębnić podłańcuch od końca tekstu do wybranego indeksu.

```Kotlin
val tekst = "To jest jeszcze inny przykładowy tekst do wyodrębnienia podłańcucha."
val podlancuch = tekst.substring(8,-10)

println(podlancuch) // wydrukuje "inny przykładowy tekst do wyodrębnienia"
```

## Deep Dive

Metoda `.substring()` pozwala nam na wygodne manipulowanie tekstem, ponieważ nie musimy samodzielnie pisać kodu do wyodrębnienia fragmentów. Jest to szczególnie przydatne, kiedy mamy do czynienia z dużym tekstem i nie chcemy ręcznie wyliczać indeksów podłańcuchów.

Warto również wspomnieć, że metoda `.substring()` jest często wykorzystywana wraz z innymi metodami, takimi jak `.indexOf()` czy `.lastIndexOf()`, które pozwalają nam na znalezienie indeksów danego znaku lub podłańcucha w tekście.

## Zobacz również

- [Oficjalna dokumentacja języka Kotlin](https://kotlinlang.org/docs/basic-syntax.html)
- [Poradnik dla początkujących w języku Kotlin](https://www.tutorialspoint.com/kotlin/index.htm)
- [Kotlin na platformie Android](https://developer.android.com/kotlin/get-started)

Dzięki metodzie `.substring()` wyodrębnianie podłańcuchów może stać się prostsze i bardziej intuicyjne. Warto więc zapoznać się z jej zastosowaniem przy pracy z tekstami w języku Kotlin.