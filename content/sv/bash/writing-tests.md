---
title:                "Skriva tester"
html_title:           "Bash: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-tests.md"
---

{{< edit_this_page >}}

### Vad & Varför?
Att skriva tester är en viktig del av programmering. Det innebär helt enkelt att skapa kod som kontrollerar och verifierar att vår huvudkod fungerar som den ska. Detta hjälper oss att upptäcka eventuella fel och buggar i vår kod tidigt, vilket leder till en mer stabil och pålitlig produkt.

### Hur man gör:
Här är ett enkelt exempel på hur man skriver ett test i Bash:

```Bash
# Skapa en funktion som adderar två tal
add() {
  echo $(($1 + $2))
}

# Testa funktionen med två olika input och förväntat output
test_add() {
  result=$(add 2 3)
  if [ $result -eq 5 ]; then
    echo "Test success: 2 + 3 = 5"
  else
    echo "Test failed: 2 + 3 != 5"
  fi

  result=$(add 5 10)
  if [ $result -eq 15 ]; then
    echo "Test success: 5 + 10 = 15"
  else
    echo "Test failed: 5 + 10 != 15"
  fi
}

# Kör testet
test_add
```

Exempel på hur detta kan se ut när du kör det:

```Bash
Test success: 2 + 3 = 5
Test success: 5 + 10 = 15
```

### Djupdykning:
Att skriva tester är en viktig del av den moderna programmeringsprocessen och har funnits under lång tid. Ursprunget till testning går tillbaka till 1970-talet med utvecklingen av enhetstester för programmeringsspråket C.

Det finns också andra verktyg för att skriva tester, som till exempel JUnit för Java och pytest för Python. Dessa verktyg erbjuder mer avancerade funktioner, men grundprincipen är densamma - att testa och verifiera vår kod.

När det gäller implementation finns det många olika strategier och praxis för att skriva tester. Det är viktigt att hitta en metod som fungerar bäst för dig och ditt team.

### Se även:
- [Test-driven development](https://en.wikipedia.org/wiki/Test-driven_development)
- [Bash man page](https://linux.die.net/man/1/bash)
- [pytest documentation](https://docs.pytest.org/en/latest/)