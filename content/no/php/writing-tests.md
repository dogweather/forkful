---
title:    "PHP: Skriving av tester"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester kan virke som en tidkrevende og kjedelig del av programmering, men det kan faktisk være svært nyttig. Tester hjelper deg med å finne og rette feil i koden din, sikrer at koden din fungerer som den skal og gjør det enklere å vedlikeholde og videreutvikle prosjektet ditt. Så hvorfor ikke lære hvordan du kan skrive gode tester?

## Hvordan

Å skrive tester i PHP kan gjøres enkelt ved hjelp av PHPUnit, som er et unit testing-verktøy for PHP. La oss se på et eksempel på hvordan du kan skrive en test for en funksjon som legger sammen to tall:

```PHP
// Tester at funksjonen `add` legger sammen to tall riktig
public function testAdd()
{
  $result = add(2, 3);

  // Forventet resultat er 5
  $this->assertEquals(5, $result);
}
```

Dette er et enkelt eksempel, men det viser hvordan du kan skrive en test og forvente et spesifikt resultat. Hvis testen feiler, vet du at det er noe som ikke fungerer som det skal og du kan gå tilbake til koden din og fikse feilen.

## Dypdykk

Nå som du har lært hvordan du kan skrive en enkel test, kan du dykke dypere ned i verdenen av testing i PHP. Det finnes ulike typer testing, som for eksempel integrasjonstesting og end-to-end testing, som du kan lære mer om. Du kan også lære om test-drevet utvikling (TDD), som er en tilnærming der du skriver testene før du skriver koden din.

Det er også viktig å forstå konseptet med code coverage, som forteller deg hvor stor del av koden din som blir testet. Dette kan være nyttig for å identifisere områder i koden som ikke er testet og som potensielt kan føre til bugs.

## Se også

- Les mer om testing i PHP [her](https://phpunit.de/).
- Utforsk ulike typer testing og TDD [her](https://www.oreilly.com/library/view/learning-phpunit-testing/9781783281183/).
- Lær mer om code coverage [her](https://phpunit.readthedocs.io/en/9.3/code-coverage-analysis.html).