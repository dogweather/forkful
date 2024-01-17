---
title:                "Interpolacja ciągu znaków"
html_title:           "Java: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

Czego dotyczy interpolacja ciągów i dlaczego programiści to robią?

Interpolacja ciągów w Java jest procesem łączenia zmiennych z wyrażeniami ciągów, aby utworzyć nowy ciąg, który można wyświetlić w wyjściu programu. Programiści często stosują tę technikę, aby uprościć kod i uniknąć powtarzania dużej ilości ciągów znaków.

Jak to zrobić:

Możesz użyć znaku "%" w celu wstawiania wartości zmiennych do wyrażenia ciągu. Na przykład:

```
String name = "Anna";
System.out.printf("Witaj, %s!", name);
```

W powyższym przykładzie używamy metody `printf` do wyświetlenia zdania zawierającego zmienną `name`. Znak "%" oznacza, że chcemy wstawić wartość zmiennej do wyrażenia ciągu, a litera `s` definiuje typ zmiennej jako `String`.

Możesz także użyć metody `String.format` do interpolacji ciągów. Na przykład:

```
String firstName = "Jan";
String lastName = "Kowalski";
String fullName = String.format("%s %s", firstName, lastName);
```

W powyższym przykładzie tworzymy nowy ciąg znaków, który składa się z imienia i nazwiska połączonych za pomocą spacji.

Pogłębione informacje:

Interpolacja ciągów znaków została wprowadzona w wersji Java 5 i jest jedną z dostępnych technik formatowania ciągów w języku Java. Inne opcje to używanie metody `String.concat` lub operatora "+" do łączenia ciągów, jednak interpolacja jest preferowaną metodą ze względu na prostotę i czytelność kodu.

Zobacz także:

Jeśli chcesz dowiedzieć się więcej o interpolacji ciągów w Java, możesz przeczytać artykuł na ten temat na stronie Oracle Java: https://www.oracle.com/technical-resources/articles/java/java-string-formatting.html.