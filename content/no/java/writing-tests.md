---
title:                "Skrive tester"
html_title:           "Java: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av en utviklers verktøykasse. Det lar deg teste koden din og sikre at den fungerer som den skal før du sender den ut i produksjon. Dette sparer tid og unngår feil i koden.

## Hvordan

For å skrive tester i Java, må du bruke et testrammeverk som JUnit eller TestNG. Disse rammeverkene lar deg strukturere testene dine og kjøre dem enkelt. Her er et eksempel på en JUnit-test som sjekker om to tall er like:

```Java
@Test
public void testEquality(){
  int num1 = 5;
  int num2 = 5;
  assertEquals(num1, num2);
}
```

I dette eksempelet bruker vi metoden `assertEquals()` som sammenligner to verdier og gir en feil hvis de ikke er like. Testene dine bør dekke alle mulige scenarier og ha en god dekning av koden din. Dette sikrer at koden din fungerer som den skal.

## Dypdykk

En viktig del av å skrive tester er å forstå hva som skal testes. Det kan hjelpe å følge en testdrevet utviklingsmetodikk, hvor du skriver testene først og deretter utvikler koden som oppfyller testene. Du bør også vurdere å bygge automatiserte tester som kan kjøres hver gang du gjør endringer i koden din.

I tillegg er det viktig å følge gode prinsipper for å skrive tester, som å sikre at de er uavhengige av hverandre og at de er enkle å vedlikeholde. En god praksis er også å skrive flere små tester i stedet for få store tester, da dette gjør feilsøking og vedlikehold lettere.

## Se Også

- [JUnit Tutorial](https://www.baeldung.com/junit-tutorial)
- [TestNG Documentation](https://testng.org/doc/documentation-main.html)
- [Test Driven Development](https://www.agilealliance.org/glossary/tdd/)