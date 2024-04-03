---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:25.035161-07:00
description: "Come fare: In Dart, \xE8 possibile utilizzare vari metodi per estrarre\
  \ sottosequenze, come `substring()`, `split()` ed espressioni regolari. Ogni metodo\u2026"
lastmod: '2024-03-13T22:44:43.117156-06:00'
model: gpt-4-0125-preview
summary: "In Dart, \xE8 possibile utilizzare vari metodi per estrarre sottosequenze,\
  \ come `substring()`, `split()` ed espressioni regolari."
title: Estrazione di sottostringhe
weight: 6
---

## Come fare:
In Dart, è possibile utilizzare vari metodi per estrarre sottosequenze, come `substring()`, `split()` ed espressioni regolari. Ogni metodo serve scopi diversi e offre flessibilità nella manipolazione delle stringhe.

### Utilizzando `substring()`:
Il metodo `substring()` è diretto. Specifichi l'indice di inizio (e opzionalmente, di fine) per tagliare la stringa.

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // Output: World
}
```

### Utilizzando `split()`:
Dividi una stringa in una lista di sottosequenze basate su un modello (come uno spazio o una virgola), e poi accedi alla sottosequenza per indice.

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // Accesso per indice
  print(result); // Output: is
}
```

### Utilizzando Espressioni Regolari:
Per modelli complessi, la classe `RegExp` di Dart è potente. Usala per abbinare modelli ed estrarre sottosequenze.

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // Output: example@mail.com
}
```

### Librerie di Terze Parti:
Sebbene la libreria standard di Dart sia piuttosto capace, potresti incontrare scenari in cui una libreria di terze parti potrebbe semplificare il tuo compito. Una scelta popolare per la manipolazione di stringhe e l'abbinamento di modelli non è specificamente consigliata qui poiché le capacità integrate di Dart spesso sono sufficienti. Tuttavia, controlla sempre [pub.dev](https://pub.dev) per eventuali librerie che potrebbero meglio soddisfare le tue esigenze specifiche.
