---
title:    "Arduino: Skriving av tester"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Hvorfor

Det å skrive tester i Arduino-programmering er en viktig praksis som kan gi mange fordeler. Det kan bidra til å forbedre koden din ved å avdekke eventuelle feil tidlig i utviklingsprosessen. Også kan testing hjelpe deg med å forstå koden bedre og gjøre det enklere å gjenbruke i fremtidige prosjekter.

# Hvordan gjøre det

For å skrive tester i Arduino, må du først inkludere biblioteket "Arduino.h". Deretter må du opprette en ny klasseløsning som arver fra "ArduinoUnit". Inne i denne klassen kan du definere ulike tester ved å bruke funksjonen "test()". Innenfor denne funksjonen kan du legge til forskjellige utsagn og forventninger, som vil sjekke om koden din fungerer som den skal.

```Arduino
#include <Arduino.h>
#include <ArduinoUnit.h>

// Ny klasse som arver fra ArduinoUnit
class TestKlasse: public ArduinoUnit{
  public:
    // Definer nye tester her
    void testEksempelfunksjon(){
      // Utsagn som skal testes
      assertTrue(10 < 20);
      // Forventninger som sjekker om 10 virkelig er mindre enn 20
    }
};

// Opprett en instans av klassen og kjør testene med run()
TestKlasse testInstanse;
void setup(){
  testInstanse.run();
}

void loop(){

}
```

Når du laster opp koden til Arduino, vil du kunne se resultatene av testene i seriellmonitor. Du vil få beskjed om hvor mange tester som har blitt kjørt, hvor mange som har passert eller feilet, og hvilke tester som har feilet.

# Dypdykk

For å skrive mer avanserte tester, kan du bruke forskjellige funksjoner som "assertEqual()" eller "assertNotEqual()" for å sammenligne variabler eller verdier. Du kan også lage flere tester i samme klasse ved å bruke flere funksjoner med forskjellige navn.

Det kan også være lurt å legge til kommentarer til testene dine for å gjøre dem mer lesbare og for å huske hva de tester. Dette kan gjøres ved å legge til en kommentar etter funksjonsnavnet, som beskriver hva testen skal sjekke.

# Se også

- [Official ArduinoUnit Library](https://github.com/mmurdoch/arduinounit)
- [Arduino Tutorial: Testing Your Code with ArduinoUnit](https://create.arduino.cc/projecthub/Arduino_Genuino/testing-your-code-a5c439)