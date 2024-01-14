---
title:                "C++: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å skrive tester når du programmerer kan virke som en ekstra og unødvendig trinn, men det er faktisk en viktig del av utviklingsprosessen. Tester hjelper deg å sikre at koden din fungerer som forventet, og kan også bidra til å identifisere og fikse feil før de blir et større problem. 

## Slik gjør du det 

Det er flere måter å skrive tester på, men vi vil fokusere på C++ språket i denne artikkelen. La oss ta for oss et enkelt eksempel på en funksjon som sjekker om et tall er et partall eller ikke: 

```C++ 
#include <iostream> 

bool erPartall(int tall) {
  if (tall % 2 == 0) {
    return true;
  } else {
    return false;
  }
}

int main() {
  int input;
  std::cout << "Skriv inn et heltall: ";
  std::cin >> input;

  if (erPartall(input)) {
    std::cout << input << " er et partall.";
  } else {
    std::cout << input << " er et oddetall.";
  }

  return 0;
}
```

I dette eksemplet bruker vi en enkel funksjon for å sjekke om et tall er et partall eller ikke. Men hvordan kan vi teste denne funksjonen for å sikre at den fungerer som den skal? Vi kan skrive en enkel testfunksjon som denne: 

```C++ 
void testErPartall() {
  assert(erPartall(2) == true);
  assert(erPartall(3) == false);
  assert(erPartall(1) == false);
  assert(erPartall(0) == true);
}

int main() {
  testErPartall();
  return 0;
}
```

Her bruker vi `assert` kommandoen for å sjekke om vår funksjon returnerer riktig verdi for ulike input. Hvis alle disse testene passerer, kan vi være sikre på at vår funksjon fungerer som den skal. Dette er spesielt nyttig hvis du gjør større endringer i koden din, da du kan kjøre testene dine igjen for å sikre at alt fortsatt fungerer som forventet.

## Dykk dypere 

Selv om dette eksemplet er enkelt, gir det et godt innblikk i hvordan du kan skrive tester for koden din. Det er også flere verktøy og rammeverk tilgjengelig for å hjelpe deg med å skrive og kjøre tester, som for eksempel Google Test og Catch2. Det er også viktig å huske på at å skrive tester ikke bare handler om å finne feil, men også om å utvikle bedre og mer pålitelig kode.

## Se også 

- [Google Test](https://github.com/google/googletest)
- [Catch2](https://github.com/catchorg/Catch2)
- [Test-Driven Development with C++](https://www.amazon.com/Test-Driven-Development-C-Kent-Beck/dp/0321146530) (bok)