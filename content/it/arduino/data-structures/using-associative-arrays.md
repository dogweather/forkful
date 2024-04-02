---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:11.971303-07:00
description: "Nel mondo di Arduino, gli array associativi ti permettono di abbinare\
  \ chiavi a valori, un po' come accoppiare i calzini con i loro compagni. Sono la\u2026"
lastmod: '2024-03-13T22:44:43.676265-06:00'
model: gpt-4-0125-preview
summary: "Nel mondo di Arduino, gli array associativi ti permettono di abbinare chiavi\
  \ a valori, un po' come accoppiare i calzini con i loro compagni. Sono la\u2026"
title: Utilizzo di array associativi
weight: 15
---

## Cosa & Perché?
Nel mondo di Arduino, gli array associativi ti permettono di abbinare chiavi a valori, un po' come accoppiare i calzini con i loro compagni. Sono la scelta prediletta quando hai bisogno di memorizzare e recuperare dati usando nomi descrittivi, rendendo il tuo codice più pulito e molto più comprensibile.

## Come fare:
Arduino, parlando in termini stretti, non ha un supporto integrato per gli array associativi come lo troveresti nei linguaggi di programmazione di alto livello. Ma, non temere. Possiamo essere ingegnosi usando strutture e array per imitare questa funzionalità. Ecco un esempio semplice per creare un "array associativo" di base per memorizzare e accedere alle temperature di diverse città.

Prima, definisci una struttura per contenere la città (chiave) e la sua temperatura (valore):

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Successivamente, inizializza un array di oggetti `CityTemperature`:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Ecco come puoi accedere e visualizzare la temperatura di una città specifica:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("La temperatura a Los Angeles è: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Niente qui per ora.
}
```

Eseguendo questo codice otterresti l'output:

```
La temperatura a Los Angeles è: 22.0
```

## Approfondimento
Storicamente, linguaggi di programmazione come C e C++ (da cui deriva la sintassi Arduino) non erano dotati di array associativi integrati, portando a soluzioni alternative come quella mostrata sopra. Questo approccio è relativamente semplice ma scala male man mano che la dimensione dei dati aumenta a causa del suo tempo di ricerca O(n).

Linguaggi come Python offrono dizionari, e JavaScript ha oggetti a questo scopo, entrambi molto più efficienti nella gestione delle coppie chiave-valore. In Arduino, quando le prestazioni e l'efficienza diventano critiche, gli sviluppatori potrebbero optare per strutture dati più specializzate, come le tabelle hash, implementate tramite librerie.

Sebbene Arduino non supporti nativamente gli array associativi, la comunità ha sviluppato librerie come `HashMap` che possono essere aggiunte al tuo progetto per fornire una funzionalità simile con prestazioni migliori rispetto a un approccio fai-da-te. Queste librerie offrono tipicamente un mezzo più elegante ed efficiente per gestire gli array associativi, specialmente per progetti più complessi.
