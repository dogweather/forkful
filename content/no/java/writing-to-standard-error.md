---
title:    "Java: Skrive til standard feil."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Hvorfor

Å skrive til standard error er en viktig del av å være en effektiv Java-programmerer. Det lar deg enkelt identifisere og håndtere feil i koden din, noe som bidrar til å gjøre programmet ditt mer robust og pålitelig.

##Slik gjør du det

Vi kan skrive til standard error ved å bruke `System.err.println()` metoden i Java. La oss se et eksempel:

```Java
public class StandardErrorDemo {

    public static void main(String[] args) {
        int result = divide(10, 0);
        System.err.println("Resultatet er: " + result);
    }

    public static int divide(int num1, int num2) {
        return num1 / num2;
    }
}
```

Når du kjører dette programmet, vil du få følgende utskrift i standard error-konsollen:
`java.lang.ArithmeticException: division by zero`

Her ser vi at programmet krasjet på grunn av et forsøk på å dele med null, og feilen ble skrevet ut i standard error-konsollen. Dette gjør det enkelt for oss å identifisere og håndtere feil i koden vår.

##Dypdykk

Å skrive til standard error er spesielt nyttig når du jobber med flertrådede applikasjoner. Standard out og standard error er separate strømmer, så ved å skrive feilmeldinger til standard error sørger vi for at de ikke blandes sammen med annen utskrift, noe som kan gjøre feilsøkingen mer utfordrende.

Det er også verdt å merke seg at standard error-konsollen vanligvis blir vist i rødt i konsollvinduet, noe som gjør det lettere å legge merke til feilmeldingene.

##Se også

- Java Error Handling: https://www.geeksforgeeks.org/exception-handling-in-java/
- Writing to Standard Error in Java: https://www.baeldung.com/java-write-to-standard-error
- The Benefits of Using Standard Error in Java: https://stackify.com/java-error-handling-best-practices/