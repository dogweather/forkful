---
title:                "Å skrive tester"
html_title:           "Swift: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-tests.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å skrive tester er en praksis innenfor programmering der man lager kode som tester at annen kode fungerer som den skal. Dette gjør at programmerere kan identifisere og løse problemer i koden sin før den tas i bruk.

# Hvordan:
```Swift
// Eksempel på en funksjon som skal teste om et tall er et partall
func erPartall(_ tall: Int) -> Bool {
    return tall % 2 == 0
}

// Testkoden som sjekker funksjonen over
let tall = 6
if erPartall(tall) {
    print("\(tall) er et partall")
} else {
    print("\(tall) er ikke et partall")
}

/* Output:
6 er et partall
*/
```

# Dypdykk:
Testdrevet utvikling (TDD) er en tilnærming som fokuserer på å skrive tester før koden, noe som kan føre til mer pålitelige og godt strukturerte programmer. Alternativer til å skrive tester inkluderer manuelt testing og kontinuerlig integrering. Implementasjonsdetaljer for tester inkluderer å bruke rammeverk som XCTest og legge til tester som en del av kontinuerlig integreringsprosessen.

# Se også:
- [Test-Driven Development in Swift](https://www.raywenderlich.com/7109-test-driven-development-in-swift)
- [XCTest Documentation](https://developer.apple.com/documentation/xctest)
- [Introduction to Unit Testing in Swift](https://medium.com/@gauravkumber/unit-testing-in-swift-36ee26b8df8b)