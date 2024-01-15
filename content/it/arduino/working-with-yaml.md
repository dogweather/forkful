---
title:                "Lavorare con yaml"
html_title:           "Arduino: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

YAML (YAML Ain't Markup Language) è un formato di dati in grado di rappresentare informazioni in un modo leggibile e facile da comprendere per gli esseri umani, ma anche facile da interpretare da parte dei computer. Utilizzare YAML come formato di configurazione nei progetti Arduino può semplificare il processo di configurazione dei dispositivi e rendere il codice più leggibile.

## Come fare

Per utilizzare YAML nei tuoi progetti Arduino, segui questi semplici passaggi: 

1. Assicurati di avere l'ultima versione di Arduino sul tuo computer. 
2. Scarica e installa la libreria "Arduino YAML Library". 
3. Apri l'IDE di Arduino e crea un nuovo sketch. 
4. Includi la libreria YAML nel tuo sketch: ```Arduino #include <YAML.h> ```
5. Usa la funzione ```Arduino YAML.load()``` per caricare il file YAML di configurazione nel tuo codice.

Ecco un esempio di codice che utilizza YAML per leggere un file di configurazione contenente informazioni su una rete Wi-Fi:

```Arduino
#include <YAML.h>

void setup() {
  // Inizializza la seriale per la comunicazione con il monitor seriale
  Serial.begin(9600);
  
  // Carica il file di configurazione YAML
  YAML.load("/config.yaml");
  
  // Leggi il nome della rete Wi-Fi dal file di configurazione
  const char* ssid = YAML["network"]["ssid"].as<const char*>();

  // Leggi la password della rete Wi-Fi dal file di configurazione
  const char* password = YAML["network"]["password"].as<const char*>();

  // Connetti al Wi-Fi
  WiFi.begin(ssid, password);

  // Attendi la connessione
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Tentativo di connessione al Wi-Fi...");
  }

  // Mostra l'indirizzo IP assegnato dal router al dispositivo
  Serial.println("Connesso al Wi-Fi! Il tuo indirizzo IP è: ");
  Serial.println(WiFi.localIP());
}

void loop() {
  // Codice del loop principale
}

```

## Approfondimento

Oltre all'utilizzo di YAML per leggere file di configurazione, è possibile utilizzarlo anche per salvare dati strutturati sulle schede SD o per comunicare con dispositivi esterni. La libreria Arduino YAML supporta anche la scrittura di file YAML, consentendo di salvare facilmente i dati del tuo progetto in un formato leggibile e modificabile. Inoltre, è possibile utilizzare variabili e strutture di dati per creare file YAML dinamici nel tuo codice.

## Vedi anche

- [Sito ufficiale di YAML](https://yaml.org/)
- [Documentazione di Arduino YAML Library](https://github.com/arduino-libraries/YAML/blob/master/README.md)
- [Esempi di utilizzo della libreria Arduino YAML](https://github.com/arduino-libraries/YAML/tree/master/examples)