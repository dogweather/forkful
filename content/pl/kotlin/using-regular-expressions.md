---
title:                "Używanie wyrażeń regularnych"
html_title:           "Kotlin: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Wykorzystanie wyrażeń regularnych jest nieodłączną częścią programowania w Kotlinie. Dzięki nim możemy znacznie ułatwić sobie pracę m.in. w zakresie walidacji danych, przetwarzania tekstów czy wyszukiwania konkretnych wzorców. Większość profesjonalnych programistów powinna umieć korzystać z tego narzędzia, aby móc pisać bardziej efektywny i czytelny kod.

## Jak to zrobić

```Kotlin
// Przykład użycia wyrażeń regularnych do sprawdzenia formatu adresu email

val regex = Regex("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,64}")
val email = "kot@example.com"

if (email.matches(regex)) {
    println("Format adresu email jest poprawny")
} else {
    println("Niepoprawny format adresu email")
}
```

W powyższym przykładzie tworzymy nowy obiekt `Regex` z wyrażeniem regularnym, które sprawdza format adresu email. Następnie, przy użyciu metody `matches()`, sprawdzamy czy podany adres `email` odpowiada temu wzorcowi. Dzięki temu możemy w prosty sposób zweryfikować, czy dany tekst spełnia założone kryteria.

```Kotlin
// Przykład użycia wyrażeń regularnych do zamiany formatu daty

val regex = Regex("(\\d{4})-(\\d{2})-(\\d{2})")
val date = "2020-08-27"
val newDate = date.replace(regex, "$3/$2/$1")

println(newDate) // Output: 27/08/2020
```

W tym przykładzie korzystamy z metody `replace()` do zamiany formatu daty z `rrrr-mm-dd` na `dd/mm/rrrr`. Wyrażenie regularne pozwala nam w prosty sposób wyodrębnić części daty i zastosować je w odpowiedniej kolejności przy użyciu operatora `$`.

## Głębszy wgląd

Wyrażenia regularne są użytecznym narzędziem, jednak mogą wymagać trochę czasu na opanowanie. W Kotlinie możemy wykorzystać je w wielu miejscach, m.in. przy filtrowaniu i przetwarzaniu danych, weryfikowaniu wprowadzonych przez użytkownika danych czy generowaniu raportów.

Warto również wspomnieć o tzw. "specjalnych sekwencjach znaków", które można wykorzystać w wyrażeniach regularnych. Na przykład, `\d` oznacza dowolną cyfrę, `\w` - dowolny znak alfanumeryczny, a `\s` - dowolny biały znak. Dzięki temu możemy szybko i precyzyjnie zdefiniować wzorce, które chcemy znaleźć lub przetworzyć.

## Zobacz również

- [Oficjalna dokumentacja języka Kotlin](https://kotlinlang.org/docs/regular-expressions.html)
- [Kotlin dla początkujących - wyrażenia regularne](https://dev.to/musialikp/kotlin-dla-poczatkujacych-wyrazenia-regularne-7pm)
- [Wprowadzenie do wyrażeń regularnych w Kotlinie](https://www.baeldung.com/kotlin-regular-expressions)