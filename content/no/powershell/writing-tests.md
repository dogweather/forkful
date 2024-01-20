---
title:                "Skriver tester"
html_title:           "PowerShell: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriver du tester i koden din? Ja, det er faktisk en viktig del av programmering! Tester er en måte å sikre at koden vår fungerer som den skal og at vi har færre feil når vi kjører den. Det sparer oss for mye tid og frustrasjon i det lange løp.

## Hvordan:
Her er et raskt eksempel på hvordan du kan skrive en enkel test i PowerShell:

```PowerShell
Function Add-Numbers ($a, $b) {
    return $a + $b
}

Describe "Add-Numbers" {
    Context "When given two numbers" {
        It "Adds them together correctly" {
            $result = Add-Numbers 2 3
            $result | Should -Be 5
        }
    }
}
```

I dette eksempelet lager vi en funksjon som legger sammen to tall, og deretter skriver vi en test for å forsikre oss om at funksjonen fungerer som den skal. Vi bruker "Describe", "Context" og "It" for å strukturere testen vår og sjekker resultatet med "Should -Be" setningen.

## Dypdykk:
Tester har blitt en viktig del av programmering siden begynnelsen av 2000-tallet. Det er mange forskjellige typer tester, som enhetstester, integrasjonstester og akseptansetester, som hver har sin egen rolle i å sikre koden vår. Alternativer til å skrive tester i PowerShell inkluderer å bruke ulike rammeverk som Pester eller NUnit.

Når du skriver tester i PowerShell, er det nyttig å ha et godt strukturert og formatert kode for å gjøre det enkelt å lese og vedlikeholde. Du kan også bruke ulike moduler og funksjoner som hjelper deg med å håndtere koden og resultatene av testene dine.

## Se også:
- [Pester documentation](https://pester.dev)
- [NUnit framework documentation](https://nunit.org)