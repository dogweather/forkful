---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:09.752457-07:00
description: "Capitalizzare una stringa significa convertire il carattere iniziale\
  \ di ogni parola nella stringa in maiuscolo, se esse sono in minuscolo, mantenendo\u2026"
lastmod: 2024-02-19 22:05:02.784567
model: gpt-4-0125-preview
summary: "Capitalizzare una stringa significa convertire il carattere iniziale di\
  \ ogni parola nella stringa in maiuscolo, se esse sono in minuscolo, mantenendo\u2026"
title: Capitalizzare una stringa
---

{{< edit_this_page >}}

## Cos'è & Perché?
Capitalizzare una stringa significa convertire il carattere iniziale di ogni parola nella stringa in maiuscolo, se esse sono in minuscolo, mantenendo inalterati i restanti caratteri. I programmatori spesso eseguono questo compito per formattare gli output, gli input degli utenti o per l'elaborazione dei dati al fine di garantire coerenza nel modo in cui il testo è presentato o elaborato, specialmente nelle interfacce utente o nei compiti di normalizzazione dei dati.

## Come fare:
In C++, è possibile capitalizzare una stringa usando la libreria standard senza la necessità di librerie di terze parti. Tuttavia, per comportamenti di capitalizzazione più complessi o specifici, librerie come Boost possono essere molto utili. Di seguito sono illustrati esempi che mostrano entrambi gli approcci.

### Usando la Libreria Standard C++:

```cpp
#include <iostream>
#include <cctype> // per std::tolower e std::toupper
#include <string>

std::string capitalizzaStringa(const std::string& input) {
    std::string risultato;
    bool capitalizzaIlProssimo = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizzaIlProssimo = true;
        } else if (capitalizzaIlProssimo) {
            ch = std::toupper(ch);
            capitalizzaIlProssimo = false;
        }
        risultato += ch;
    }

    return risultato;
}

int main() {
    std::string testo = "hello world from c++";
    std::string testoCapitalizzato = capitalizzaStringa(testo);
    std::cout << testoCapitalizzato << std::endl; // Output: "Hello World From C++"
}
```

### Usando la Libreria Boost:

Per manipolazioni di stringhe più avanzate, inclusa la capitalizzazione consapevole delle impostazioni locali, potresti voler usare la libreria Boost String Algo.

Prima di tutto, assicurati di avere la libreria Boost installata e configurata nel tuo progetto. Poi puoi includere gli header necessari e usare le sue funzionalità come mostrato di seguito.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string testo = "hello world from c++";
    std::string testoCapitalizzato = testo;

    // capitalizza la prima lettera di ogni parola
    boost::algorithm::to_lower(testoCapitalizzato); // assicurandosi che la stringa sia in minuscolo
    testoCapitalizzato[0] = std::toupper(testoCapitalizzato[0]); // capitalizza il primo carattere

    for (std::size_t i = 1; i < testoCapitalizzato.length(); ++i) {
        if (isspace(testoCapitalizzato[i - 1])) { // capitalizza dopo uno spazio
            testoCapitalizzato[i] = std::toupper(testoCapitalizzato[i]);
        }
    }

    std::cout << testoCapitalizzato << std::endl; // Output: "Hello World From C++"
}
```

In questo caso, Boost semplifica alcune delle attività di manipolazione delle stringhe ma richiede comunque un approccio personalizzato per una vera capitalizzazione, poiché offre principalmente utilità di trasformazione e conversione di maiuscolo/minuscolo.
