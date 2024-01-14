---
title:                "C: Utskrift av feilsøkingsutdata"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/printing-debug-output.md"
---

{{< edit_this_page >}}

Så du har mest sannsynlig brukt mye tid på å skrive og feilsøke din C-kode, og nå lurer du kanskje på om det er nødvendig å legge til debug-utskrift for å forbedre prosessen din. Vel, la meg fortelle deg at svaret er enkelt - ja! Utskrift av feilsøking kan være en uvurderlig ressurs for å finne og rette opp feil i koden din. La oss dykke inn i hvorfor og hvordan du bør inkludere debug-utskrift i din programmeringspraksis.

## Why
Å legge til debug-utskrift i koden din kan hjelpe deg med å forstå og identifisere feil i logikken din. Det kan også være en nyttig måte å spore variabler og data som kan bidra til å åpne dørene for videre feilsøking. Med debug-utskrift kan du finne ut nøyaktig hvor og når en feil oppstår, noe som kan være spesielt nyttig når du støter på feil som bare oppstår under visse betingelser eller med bestemte data.

## How To
La oss se på et eksempel på hvordan du kan legge til debug-utskrift i koden din:
```C
// Dette er en enkel funksjon for å multiplisere to tall
int multiply(int num1, int num2) {
  int result = num1 * num2;
  printf("Result of multiplying %d and %d is %d\n", num1, num2, result); // debug-utskrift
  return result;
}

int main() {
  // Kall på funksjonen og lagre resultatet i en variabel
  int res = multiply(5, 7);
  
  // Bruk resultatet videre i koden din
  printf("Resultatet av multiplikasjonen er: %d\n", res);
  
  return 0;
}
```

Som du kan se, legger vi til debug-utskrift i funksjonen vår for å se hva resultatet av multiplikasjonen faktisk er. Ved å se på utskriften, kan vi bekrefte at funksjonen vår fungerer som forventet og videre spore eventuelle feil som kan oppstå.

## Deep Dive
Det er noen tips og triks å huske på når du legger til debug-utskrift i koden din:
- Sørg for å fjerne eller deaktivere alle debug-utskrifter før du sender koden til produksjon.
- Hold utskriftene enkle og klare, slik at de er enkle å lese og forstå.
- Bruk variabelnavn og relevante tekster i dine debug-utskrifter for å gjøre feilsøkingsprosessen enklere.

Se også
- [The Art of Debugging in C](https://medium.com/@deepu105/the-art-of-debugging-in-c-6c916bdcb85f)
- [Debugging Basics in C Programming](https://www.freecodecamp.org/news/debugging-basics-in-c-programming/)
- [Using Debugging Tools in C](https://docs.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2013/8y2sy99b(v=vs.120))