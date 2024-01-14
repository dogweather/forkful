---
title:    "C#: Å skrive tester"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang følt deg frustrert når du har funnet en bug i koden din? Eller kanskje flere bugs som dukker opp etter en endring? Dette er hvorfor det er så viktig å skrive tester for koden din. Ved å skrive tester kan du effektivt finne og fikse bugs før de når produksjon, noe som sparer deg for både tid og hodebry.

## Hvordan

For å skrive tester i C# trenger du en testrammeverk som NUnit eller xUnit. Disse verktøyene gjør det enkelt å kjøre testene dine og rapportere resultater. La oss se på et eksempel på hvordan du kan skrive en enkel test i NUnit:

```C#
[Test]
public void TestAddition()
{
    int result = Calculator.Add(2, 3);

    Assert.AreEqual(5, result);
}
```

Her ser vi på en test som sjekker om funksjonen "Add" i en kalkulator gir riktig resultat når vi legger sammen 2 og 3. Vi bruker Assertions for å sammenligne verdien vi får fra funksjonen med forventet resultat. Hvis testen feiler, vil NUnit rapportere det og du kan da gå tilbake og gjøre nødvendige endringer.

Selv om dette eksempelet er veldig enkelt, kan tester også være mye mer komplekse ved å sjekke forskjellige scenarier og grensetilfeller. Det viktigste er å tenke på hva slags input og output du forventer fra funksjonen din, og skrive tester som sjekker det.

## Dypdykk

Det er flere typer tester du kan skrive i C#, for eksempel enhetstester, integrasjonstester og akseptansetester. Enhets- og integrasjonstester er vanligvis skrevet av utviklere, mens akseptansetester er skrevet av for eksempel kvalitetssikringsavdelingen. Noen fordeler med å skrive tester i C# er at de kan kjøres automatisk og fange bugs før de når produksjon, at de kan brukes som dokumentasjon for å forstå en funksjon, og at de er en god måte å sikre kvaliteten på koden din.

## Se også

- [NUnit](https://nunit.org/)
- [xUnit](https://xunit.net/)
- [En guide til å skrive gode tester i C# (engelsk)](https://stackify.com/unit-testing-basics-best-practices/)
- [En grundig introduksjon til enhetstesting i C# (engelsk)](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [En video om å skrive enhetstester i C# (engelsk)](https://www.youtube.com/watch?v=ub3PcUlz1Cg)