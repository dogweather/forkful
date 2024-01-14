---
title:                "Java: Skrive tester"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi vet alle hvor viktig det er å skrive god og feilfri kode når vi utvikler programvare. Men en like viktig del av utviklingsprosessen er å skrive tester for å sikre at koden vår fungerer som den skal. Tester hjelper oss med å oppdage og fikse feil før de kommer ut til sluttbrukeren, og bidrar til å skape mer robuste og pålitelige programmer. I denne bloggposten kommer vi til å se på hvorfor det er viktig å skrive tester i Java-programmering.

## Hvordan

Skriving av tester kan virke som en tidkrevende prosess, men det er absolutt verdt innsatsen i det lange løp. La oss se på et enkelt eksempel for å demonstrere hvordan tester kan bidra til å forbedre koden vår. Vi skal lage en enkel kalkulator som kan legge sammen to tall og returnere resultatet. Her er koden vår:

```java
public static int add(int a, int b){
  return a + b;
}
```

For å teste denne koden, må vi skrive noen tester som sjekker om add-metoden returnerer riktig resultat for forskjellige input-verdier. Her er et eksempel på en test:

```java
@Test
public void testAdd() {
  int result = add(5, 10);
  int expected = 15;
  assertEquals(expected, result);
}
```

Vi kjører testen og ser at den passerer, alt fungerer som forventet. Men hva hvis vi hadde glemt å ta hensyn til negativ input? Ved å skrive flere tester, for eksempel en for negativ input, kan vi oppdage og fikse denne feilen før den skaper problemer i produksjon.

## Dypdykk

Det finnes ulike typer tester i Java-programmering, som enhetstester, integrasjonstester og akseptansetester. Enhets- og integrasjonstester blir vanligvis skrevet av utviklere, mens akseptansetester blir skrevet av personer utenfor utviklingsteamet. Enhets- og integrasjonstester er viktig for å sikre at koden vår fungerer som den skal, mens akseptansetester hjelper oss med å sikre at applikasjonen tilfredsstiller krav fra brukerne.

Når du skriver tester, er det viktig å ha god dekning av koden din. Det betyr å sørge for at alle kodegrenene dine blir testet, og at du tester ulike inngangsverdier. Ved å ha god testdekning, sikrer du deg at din kode fungerer som forventet og at eventuelle feil blir oppdaget og fikset.

En annen viktig faktor for å skrive gode tester er å velge et godt rammeverk. Det finnes ulike testrammeverk tilgjengelig for Java-programmering, som JUnit og TestNG. Disse rammeverkene gjør det enklere å skrive og kjøre tester, og gir deg nyttig informasjon om testresultatene.

## Se Også

- [Java Testing Tutorial: How to Write Unit Tests With JUnit](https://www.baeldung.com/junit-test-classes)
- [Introduction to Test-Driven Development (TDD) in Java](https://stackify.com/test-driven-development-tdd-java/)
- [Testing Java Applications with TestNG](https://www.baeldung.com/spring-mvc-test-with-testng)
- [Effective Test Coverage: What It Is and How to Measure It](https://dzone.com/articles/effective-test-coverage-what-it-and-how-to-measure)