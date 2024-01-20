---
title:                "Skriva tester"
html_title:           "PowerShell: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/writing-tests.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Skrivning av tester är en viktig del av programmeringsprocessen. Det innebär att skriva kod som kollar att vår huvudkod fungerar som den ska. Detta gör att vi kan vara säkra på att vårt program fungerar som förväntat och undvika oväntade fel när vi släpper det till användarna.

# Hur man:

```PowerShell
Describe "Addition" {
  it "adds two numbers" {
    $sum = 1 + 1
    $sum | Should Be 2
  }
}
```
```PowerShell
$Results = Invoke-Pester -Script .\MyScript.ps1
$Results.FailedCount # outputs the number of failed tests
```

# Fördjupning:

(1) Tester har funnits i programmering sedan tidigaste dagar och hjälper oss att identifiera och förhindra buggar i vår kod. (2) Ett alternativ till Pester (det vanligaste testramverket för PowerShell) är platshållare som TestScript och RunspaceFactory. (3) I implementationsdetaljer använder vi vanligtvis Verify-Something funktioner för att jämföra resultatet av vår kod med det förväntade resultatet.