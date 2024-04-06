---
date: 2024-01-20 17:51:49.286378-07:00
description: "How to: (Come fare:) La stampa di debug risale agli albori dell'informatica.\
  \ In alternativa, si possono usare LED o display per fornire feedback visivo.\u2026"
lastmod: '2024-04-05T22:50:57.488626-06:00'
model: gpt-4-1106-preview
summary: (Come fare:) La stampa di debug risale agli albori dell'informatica.
title: Stampa dell'output di debug
weight: 33
---

## How to: (Come fare:)
```Arduino
void setup() {
  Serial.begin(9600); // Avvia la comunicazione seriale
}

void loop() {
  Serial.println("Hello, Debug!"); // Stampa il messaggio di debug
  delay(1000); // Aspetta 1 secondo
}
```
Output:
```
Hello, Debug!
Hello, Debug!
...
```

## Deep Dive (Approfondimento)
La stampa di debug risale agli albori dell'informatica. In alternativa, si possono usare LED o display per fornire feedback visivo. L'uso di `Serial` è comodo ma richiede un cavo USB; sistemi wireless come Bluetooth o WiFi possono esser usati per debug senza fili.

## See Also (Vedi Anche)
- [La documentazione ufficiale di Arduino sulle comunicazioni Seriali](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Discussioni sul forum di Arduino su tecniche di debug](https://forum.arduino.cc/)
