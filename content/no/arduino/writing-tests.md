---
title:    "Arduino: Skrive tester"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor

Når du lager Arduino programmer, ønsker du sannsynligvis å sikre at koden din fungerer som forventet. Dette kan være spesielt viktig hvis du skal bruke koden din i et prosjekt eller for en spesiell funksjon. Å skrive tester er en måte å sikre at koden din fungerer som den skal, og å oppdage eventuelle feil før de faktisk oppstår. På denne måten kan du være trygg på at koden din vil fungere som forventet når du trenger den.

# Hvordan

For å skrive tester for dine Arduino programmer, kan du bruke biblioteket "ArduinoUnit". Først må du laste ned biblioteket og importere det i Arduino IDE. Deretter kan du bruke følgende kode for å skrive en enkel test som sjekker om et gitt tall er større enn et annet tall:

```
ArduinoUnit> expect(3 > 2);
```

Output fra denne testen vil vise "OK" hvis testen passerer, eller "FAILED - expecting 3 to be greater than 2" hvis testen feiler.

Du kan også skrive mer avanserte tester ved å bruke tilleggsfunksjoner som "assertEqual" for å sammenligne verdier eller "assertRaises" for å teste om en spesifikk funksjon utløser en forventet feil.

# Dypdykk

For de som ønsker å gå dypere inn i testing av Arduino programmer, finnes det flere ressurser tilgjengelig. Du kan lære mer om hvordan du bruker biblioteket "ArduinoUnit" ved å se på dokumentasjonen og eksemplene som følger med. Du kan også se på ulike teknikker for testing av Arduino-kode, for eksempel "mocking" eller "integration testing". Dette vil hjelpe deg med å finne den beste tilnærmingen for testing av dine egne Arduino prosjekter.

# Se også

- [ArduinoUnit biblioteket](https://github.com/mmurdoch/arduinounit)
- [Offisiell Arduino programmeringsguide](https://www.arduino.cc/en/Guide/Introduction)
- [Dokumentasjon for ArduinoUnit](https://arduinounit.github.io/ArduinoUnit/)
- [Eksempler på testing av Arduino-kode](https://www.makeuseof.com/tag/unit-testing-arduino-code/)