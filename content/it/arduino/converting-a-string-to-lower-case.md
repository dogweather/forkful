---
title:                "Arduino: Conversione di una stringa in minuscolo"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Molte volte ci troviamo di fronte alla necessità di convertire una stringa in minuscolo nel nostro codice Arduino. Questo può essere utile per confrontare due stringhe in maniera più precisa o per gestire i dati in modo uniforme. In questa guida impareremo come eseguire questa operazione in modo semplice ed efficiente.

## Come fare

```Arduino
String stringa = "TEStO Di ProVa"; // Stringa originale
stringa.toLowerCase(); // Converte la stringa in minuscolo

Serial.println(stringa); // Stampa "testo di prova" sulla serial monitor
```

In questo esempio, la funzione `toLowerCase()` viene utilizzata per convertire la variabile `stringa` in minuscolo. È importante notare che questa funzione modifica la stringa originale, quindi assicurati di avere una copia di backup se necessario.

Un altro esempio può essere quello di utilizzare la funzione `toLowerCase()` all'interno di un loop per gestire l'input utente:

```Arduino
Serial.println("Inserisci una parola in maiuscolo: ");
while (Serial.available() == 0); // Attendiamo finché l'utente non inserisce qualcosa

String input = Serial.readString(); // Legge la stringa inserita dall'utente
Serial.print("La parola in minuscolo è: ");
Serial.println(input.toLowerCase()); // Converte la parola in minuscolo e la stampa sulla serial monitor
```

Nell'esempio sopra, l'utente inserisce una parola in maiuscolo e tramite la funzione `toLowerCase()`, viene convertita in minuscolo e stampata sulla serial monitor.

## Approfondimento

Se abbiamo bisogno di gestire stringhe con caratteri speciali o accentati, potremmo dover utilizzare la libreria `ctype.h` per avere una conversione corretta. Ad esempio:

```Arduino
#include <ctype.h>

String stringa = "CIAO!";
for (int i = 0; i < stringa.length(); i++) {
  stringa[i] = tolower(stringa[i]); // Utilizza la funzione tolower() per convertire ogni carattere della stringa in minuscolo
}
Serial.println(stringa); // Stampa "ciao!" sulla serial monitor
```

La funzione `tolower()` è definita nella libreria `ctype.h` e ci permette di convertire ogni carattere della stringa in minuscolo, anche quelli speciali o accentati.

## Vedi anche

- [Guida alle stringhe in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/toascii/)
- [Libreria ctype.h di Arduino](https://www.arduino.cc/reference/en/libraries/ctype/)
- [Tutorial su come utilizzare le stringhe in Arduino](https://www.tutorialspoint.com/arduino/arduino_strings.htm)