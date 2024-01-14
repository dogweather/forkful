---
title:                "Java: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en avgjørende del av Java-programmering som kan bidra til å forbedre koden din og sikre at den fungerer som den skal. Tester er også nyttige for å finne og fikse feil i koden din, noe som kan spare deg for mye hodepine i det lange løp.

## Hvordan

For å skrive tester i Java, må du følge disse trinnene:

1. Definer et nytt testklasse ved å bruke `@Test`-annotasjonen.
2. Sett opp data eller objekter som skal testes.
3. Lag en testmetode ved å bruke `@Test`-annotasjonen.
4. Bruk `assert`-metoder til å bekrefte at den forventede utgangen er den samme som den faktiske utgangen.

Et eksempel på en testklasse og testmetode kan se ut som dette:

```Java
@Test
public class CalculatorTest {
  Calculator calculator = new Calculator();
  
  @Test
  public void testAddition() {
    int result = calculator.add(2, 2);
    assert result == 4;
  }
}
```
I dette eksempelet opprettes det en testklasse kalt `CalculatorTest` som tester om `add`-metoden i `Calculator`-klassen fungerer som den skal. I testmetoden brukes `assert`-metoden for å bekrefte at resultatet av addisjonen er 4, som er det forventede resultatet.

Det finnes også flere rammeverk som kan hjelpe deg med å skrive og kjøre tester, som for eksempel JUnit og TestNG.

## Deep Dive

Når du skriver tester, er det viktig å huske på at de skal være enkle å forstå og vedlikeholde. Det betyr at du bør unngå å skrive for mange tester for én funksjon, og heller fokusere på å skrive effektive tester som dekker forskjellige scenarioer.

I tillegg er det lurt å inkludere både suksesscenarioer og feilscenarioer i testene dine. Dette vil bidra til å avdekke eventuelle feil og sørge for at koden din er robust og pålitelig.

En annen viktig ting å huske på er at tester skal være uavhengige av hverandre. Det betyr at en test ikke skal være avhengig av resultatet av en annen test. Dette sikrer at testene dine gir nøyaktige resultater og at eventuelle feil ikke påvirker andre tester.

## Se også

- [JUnit Tutorial](https://www.petrikainulainen.net/programming/testing/junit-5-tutorial-writing-our-first-test-class/)
- [TestNG Documentation](https://testng.org/doc/)
- [TDD vs BDD: What's the difference?](https://www.altexsoft.com/blog/testing-tdd-vs-bdd/)