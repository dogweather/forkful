---
title:                "Java: Sammenligning av to datoer"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

Sammenligning av to datoer er en viktig del av Java-programmering. Det hjelper deg med å håndtere datoer og tidsstempel i din kode. Enten du jobber med å lage et tidsstempel for en bestilling eller beregne alderen til en bruker, er sammenligning av datoer en nøkkelkomponent. La oss se på hvordan du kan gjøre dette i Java.

# Hvordan

For å sammenligne to datoer i Java, kan du bruke klassen "LocalDate" fra "java.time" pakken. La oss si at du har to datoer som du vil sammenligne:

```Java
LocalDate dato1 = LocalDate.of(2021, 5, 23);
LocalDate dato2 = LocalDate.of(2021, 5, 25);
```

For å sammenligne disse datoene, kan du bruke metoden "compareTo" som returnerer en int-verdi. Hvis den første datoen er tidligere enn den andre, vil verdien være negativ. Hvis den første datoen er senere enn den andre, vil verdien være positiv. Hvis datoene er like, vil verdien være null.

```Java
int sammenligning = dato1.compareTo(dato2);

if (sammenligning < 0) {
    System.out.println("Dato 1 er tidligere enn dato 2");
} else if (sammenligning > 0) {
    System.out.println("Dato 1 er senere enn dato 2");
} else {
    System.out.println("Dato 1 og dato 2 er like");
}

// Output: Dato 1 er tidligere enn dato 2
```

Du kan også bruke den innebygde metoden "isEqual" for å sjekke om to datoer er like.

```Java
if (dato1.isEqual(dato2)) {
    System.out.println("Dato 1 og dato 2 er like");
} else {
    System.out.println("Dato 1 og dato 2 er ikke like");
}

// Output: Dato 1 og dato 2 er ikke like
```

# Dypdykk

Når du sammenligner datoer, må du være oppmerksom på at datoer også inneholder en tidssone. Derfor kan to datoer som ser forskjellige ut, faktisk være like når du tar hensyn til tidssonen.

Du kan også sammenligne datoer basert på forskjellige faktorer som år, måned eller dag. For å gjøre dette kan du bruke metoden "isBefore" eller "isAfter". Disse metodene sjekker om den første datoen kommer før eller etter den andre datoen basert på den spesifiserte faktoren.

```Java
if (dato1.isBefore(dato2)) {
    System.out.println("Dato 1 kommer før dato 2");
} else {
    System.out.println("Dato 1 kommer etter dato 2");
}

// Output: Dato 1 kommer før dato 2
```

# Se også

- Java Offisiell Dokumentasjon - [Compare Dates in Java](https://docs.oracle.com/javase/tutorial/datetime/iso/compare.html)
- TutorialsPoint - [Java LocalDate Class](https://www.tutorialspoint.com/java8/java8_localdate.htm)
- W3Schools - [Java Date and Time](https://www.w3schools.com/java/java_date.asp)