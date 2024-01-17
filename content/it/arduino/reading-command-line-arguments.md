---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Arduino: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere gli argomenti della riga di comando è il processo di estrarre e utilizzare le informazioni fornite dall'utente al momento dell'esecuzione di un programma. I programmatori lo fanno per personalizzare il comportamento del loro programma o per fornire input dinamici.

## Come fare:
Per leggere gli argomenti della riga di comando in Arduino, è possibile utilizzare la funzione ```main``` e l'array ```argv```. Di seguito è riportato un esempio di codice che stampa l'input fornito dall'utente:

```Arduino
void setup() {
  Serial.begin(9600); // inizializza la comunicazione seriale a 9600 bps
}

void loop() {
  Serial.println(argv[1]); // stampa il primo argomento fornito dall'utente
}
```

Se si esegue questo codice con l'input "Hello World", si otterrà l'output "Hello". Per leggere più argomenti, è possibile utilizzare un loop:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  for(int i = 0; i < argc; i++) {
    Serial.println(argv[i]); // stampa ogni argomento fornito dall'utente
  }
}
```

## Approfondimento:
Leggere gli argomenti della riga di comando è un concetto comune nella programmazione e viene utilizzato in vari linguaggi di programmazione, come C e Java. Consente ai programmatori di rendere i loro programmi più personalizzabili e flessibili per l'utente finale.

Un'alternativa all'utilizzo di ```argv``` è l'utilizzo della funzione ```Serial.readString()```, che consente di leggere direttamente l'input dell'utente dalla porta seriale. Tuttavia, questo metodo richiede che l'utente interagisca con il programma attraverso un'interfaccia seriale.

## Vedi anche:
- Documentazione ufficiale di Arduino su ```argv```: https://www.arduino.cc/reference/en/language/variables/environment/argc/
- Tutorial su come leggere gli argomenti della riga di comando in Arduino: https://create.arduino.cc/projecthub/Aritra/how-to-read-commands-from-command-line-0cbe12