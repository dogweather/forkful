---
title:                "Java: Konwersja daty na ciąg znaków"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu potrzebujemy konwertować datę na łańcuch znaków, aby móc wyświetlić ją użytkownikowi w czytelnej formie lub przekazać jako argument do innej funkcji. Jest to bardzo powszechne zadanie, dlatego w tym artykule opowiemy o tym, jak to zrobić w języku Java.

## Jak to zrobić

Konwersja daty na łańcuch znaków w Javie jest bardzo prosta i wymaga użycia klasy DateFormat oraz jej metody format. To pozwala na zdefiniowanie odpowiedniego formatu dla daty i przekonwertowanie jej do żądanej postaci. Poniżej znajduje się przykładowy kod w języku Java oraz jego wynik:

```java
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class FormatowanieDaty {

    public static void main(String[] args) {
        Date data = new Date();
        DateFormat format = new SimpleDateFormat("dd/MM/yyyy");
        
        String dataJakoString = format.format(data);
        
        System.out.println(dataJakoString);
    }
}
```

```
07/08/2021
```

W powyższym przykładzie użyliśmy klasy SimpleDateFormat, aby określić format daty jako "dzień/miesiąc/rok" i przekonwertować bieżącą datę do łańcucha znaków.

## Deep Dive

Jeśli chcesz dowiedzieć się więcej na temat konwertowania daty na łańcuch znaków w Javie, to polecamy zapoznać się z dokumentacją klasy DateFormat oraz jej różnych implementacji, takich jak SimpleDateFormat czy DateTimeFormatter. W niektórych przypadkach, konwersja daty może wymagać użycia również metody parse, która pozwala na przetworzenie łańcucha znaków w datę.

## Zobacz również

- [JavaDocs: DateFormat (javadoc.io)](https://javadoc.io/doc/javax.xml.bind/jaxb-api/2.2.12/javax/xml/bind/DatatypeConverter.html)
- [JavaDocs: SimpleDateFormat (javadoc.io)](https://javadoc.io/doc/java.base/java/text/SimpleDateFormat.html)
- [JavaDocs: DateTimeFormatter (javadoc.io)](https://javadoc.io/doc/java.base/java/time/format/DateTimeFormatter.html)