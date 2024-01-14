---
title:    "PHP: Skriving av tester"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av PHP programmering fordi det hjelper oss å oppdage eventuelle feil eller bugs i koden vår. Dette gjør det enklere å feilsøke og å sikre at koden fungerer som tiltenkt.

## Hvordan

En måte å skrive tester i PHP er ved å bruke et populært rammeverk som PHPUnit. La oss se på et enkelt eksempel på hvordan vi kan skrive en test for en funksjon som legger sammen to tall.

```PHP
// Funksjonen vi vil teste
function addNumbers($x, $y) {
  return $x + $y;
}

// Testen vår
public function testAddNumbers() {
  // Arrange
  $x = 5;
  $y = 10;

  // Act
  $result = addNumbers($x, $y);

  // Assert
  $this->assertEquals(15, $result);
}
```

Her ser vi at vi først definerer en funksjon som vi ønsker å teste, og deretter skriver vi en testfunksjon ved hjelp av `assertEquals` for å sammenligne forventet resultat med faktisk resultat. Ved å bruke `Arrange`, `Act` og `Assert` strukturen, kan vi enkelt definere testene våre og sørge for at koden vår fungerer som den skal.

## Dykk dypere

Å skrive tester i PHP er en omfattende emne, og det er mange ulike måter å gjøre det på. For eksempel kan du også bruke `expectException` for å sjekke om koden din kaster et unntak når det er forventet. Det er også lurt å skrive såkalte "negative" tester hvor vi sjekker om koden vår håndterer ugyldige verdier eller situasjoner på riktig måte.

Det er også viktig å huske på å skrive tester for både positive og negative scenarier for å sikre at koden vår fungerer i alle tilfeller. I tillegg kan vi også bruke mock-objekter og løsne avhengigheter for å gjøre testingen mer effektiv og isolere koden vår.

## Se også

- [PHPUnit dokumentasjon](https://phpunit.de/documentation.html)
- [En enkel guide til testing i PHP](https://medium.com/@CoderVlogger/testing-with-phpunit-for-beginners-level-1-d05e5c1ffdfc)
- [Hvorfor bør du skrive tester for koden din?](https://www.freecodecamp.org/news/why-you-should-learn-how-to-test-your-code-and-how-to-do-it-c9cbb3adc0ba/)