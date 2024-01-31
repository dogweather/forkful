---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
date:                  2024-01-20T17:41:19.599089-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Eliminare caratteri che corrispondono a un certo schema significa rimuovere specifici elementi da una stringa, per pulirla o per estrarre dati essenziali. I programmatori lo fanno per gestire meglio le informazioni e ridurre errori.

## Come fare:
```Arduino
void setup() {
  Serial.begin(9600);
  String data = "B4n4n4s4Lif3!";
  String pattern = "4";
  data = deleteMatchingChars(data, pattern);
  Serial.println(data); // Output: BnnnsLf!
}

void loop() {
  // Nothing to do here
}

String deleteMatchingChars(String str, String pattern) {
  for (int i = 0; i < pattern.length(); i++) {
    str.replace(String(pattern[i]), "");
  }
  return str;
}
```
## Approfondimento
La rimozione di caratteri corrispondenti in una stringa non è una funzione nativa dei linguaggi di programmazione più antichi, ma si è evoluta con l'introduzione delle espressioni regolari (regex). Alternativamente, si potrebbero usare funzioni di manipolazione delle stringhe come `replace` o iterare manualmente attraverso ogni carattere. Dettagli d'implementazione variano a seconda del linguaggio: in C, ad esempio, è necessario manipolare gli array di caratteri manualmente, mentre linguaggi più nuovi offrono metodi più efficienti.

## Vedi anche
- [Arduino Reference: StringObject](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Reference: StringReplace](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Regular Expressions in C++](https://www.cplusplus.com/reference/regex/) - Sebbene non sia direttamente correlato all'Arduino, fornisce ulteriori informazioni sulle regex in uno dei linguaggi su cui si basa Arduino.
