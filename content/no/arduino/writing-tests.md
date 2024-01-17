---
title:                "Å skrive tester"
html_title:           "Arduino: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester er en viktig del av programmering. Det er en måte å sjekke om koden vår fungerer som den skal, og det hjelper oss med å finne og fikse feil før de blir et problem for brukeren.

## Hvordan:
For å skrive tester i Arduino, kan du bruke funksjonen ```test ()```. Denne funksjonen lar deg teste ulike aspekter av koden din og gi deg et resultat, for eksempel om det er feil eller om alt fungerer som det skal. Her er et eksempel på hvordan du kan bruke ```test ()``` funksjonen:

```Arduino
// Eksempeltest
void setup(){
  Serial.begin(9600);
}

void loop(){
  int a = 5;
  int b = 10;
  int sum = a + b;
  
  int test_result = test(sum); // Bruk av test() funksjonen
  
  Serial.println(test_result); // Skrive ut testresultatet i serieporten
}

// Funksjon for å teste om summen er riktig
int test(int sum){
  if (sum == 15){ // Hvis summen er riktig
    return 1; // Returnere 1 (true)
  }
  else{
    return 0; // Returnere 0 (false)
  }
}
```

Eksempelutdata fra denne koden vil være "1" siden summen er riktig og testen ble bestått.

## Gå i dybden:
Skriving av tester har blitt en viktig del av programmering de siste årene. Tidligere var det vanlig å bare teste koden manuelt, men med større og mer komplekse programmer har det blitt essensielt å kunne automatisk teste koden vår. Det finnes også andre måter å skrive tester på, for eksempel ved å implementere enhetstester eller integrasjonstester. Disse kan hjelpe oss med å finne problemer på et mer spesifikt nivå.

En annen viktig del av å skrive tester er å sørge for at de er pålitelige og gir oss riktig resultat. Dette kan gjøres ved å følge "about face" prinsippet (Also Know As: "clear box testing"), som betyr at vi tester hver enkelt del av koden individuelt for å sikre at det fungerer korrekt.

Mer om å skrive tester i Arduino kan du lese om [her](https://www.arduino.cc/en/Guide/ArduinoTestEnvironment).

## Se også:
For mer informasjon om å skrive tester i Arduino, kan du sjekke ut [Arduino Testing Library](https://github.com/JChristensen/arduino-test). Denne inneholder nyttige funksjoner for å skrive tester, og du kan også bidra og forbedre biblioteket.

Ønsker du å lære mer om automatisert testing i Arduino, kan du se [denne videoen](https://www.youtube.com/watch?v=vWU-oPfB-ys). Den viser hvordan du kan sette opp et testmiljø og skrive dine egne tester.

Husk at å skrive tester i Arduino er en nyttig og viktig del av programmering. Det kan hjelpe deg med å skrive bedre og mer pålitelige programmer, og spare deg for mye tid og frustrasjon på sikt. Så ikke nøl med å implementere tester i ditt neste prosjekt!