---
title:                "Skriving av tester"
html_title:           "C++: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-tests.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Å skrive tester er en viktig del av programmeringsprosessen. Det betyr å skrive små deler av kode som sjekker om den større delen av koden fungerer som den skal. Testene hjelper til med å identifisere feil tidlig, slik at de kan bli fikset før de blir et større problem.

# Hvordan:
```C++
#include <iostream>

// En funksjon som tar inn to tall og returnerer summen av dem
int add(int a, int b){
  return a+b;
}

// En test for å sjekke om add() funksjonen returnerer riktig resultat
void test_add(){
  int result = add(3, 5); 
  if(result == 8){
    std::cout << "Test passed!" << std::endl;
  } else {
    std::cout << "Test failed!" << std::endl;
  }
}

int main(){
  test_add(); // Kaller på testen for å sjekke om funksjonen virker som den skal
}
```

Output: Test passed!

# Dypdykk:
Å skrive tester har blitt en standard praksis i moderne programmering, spesielt med introduksjonen av testdrevet utvikling (TDD). TDD betyr å skrive testene før du skriver selve koden, og deretter skrive kode som består testene. Dette bidrar til å produsere mer pålitelig og feilfri kode.

Noen programmerere foretrekker å skrive tester etter at koden allerede er skrevet. Dette kalles for retroaktiv testdrevet utvikling (RetroTDD). Begge tilnærminger kan hjelpe til med å identifisere og fikse feil tidlig i prosessen.

Det finnes også verktøy som kan automatisk generere tester basert på koden din, som for eksempel Google Test og CppUnit. Disse kan være nyttige for mer komplekse prosjekter.

# Se også:
- https://www.guru99.com/test-driven-development.html
- https://www.jetbrains.com/help/clion/unit-testing-tutorial.html
- https://github.com/google/googletest