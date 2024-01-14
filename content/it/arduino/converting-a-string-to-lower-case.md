---
title:    "Arduino: Conversione di una stringa in minuscolo"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Perché convertire una stringa in minuscolo con Arduino?

La programmazione di Arduino può sembrare complicata, ma imparare a convertire una stringa in minuscolo è un'abilità fondamentale che può dare molte opportunità di personalizzazione ai tuoi progetti. Con la semplice manipolazione di testo, puoi creare comandi vocali più naturali, visualizzare dati in modo uniforme o semplicemente rendere il tuo codice più leggibile.

## Come fare

Per convertire una stringa in minuscolo su Arduino, abbiamo bisogno di utilizzare la funzione `toLowerCase()` (tradotto come `minuscolo()` in italiano). Questa funzione prende una stringa come argomento e la converte in minuscolo. Ecco un esempio di codice e l'output corrispondente:

```Arduino
String testo = "QUESTO è UN ESEMPIO";
Serial.println(testo.minuscolo()); //output: questo è un esempio
```

Puoi anche utilizzare la funzione `tolower()` per convertire singoli caratteri all'interno di una stringa. Ad esempio:

```Arduino
String testo = "Questo Esempio";
String risultato = "";
for (int i = 0; i < testo.length(); i++) {
  char carattere = testo[i];
  risultato += tolower(carattere); //aggiunge il carattere convertito al risultato
}
Serial.println(risultato); //output: questo esempio
```

## Approfondimenti

Oltre alla funzione `toLowerCase()`, esistono altre opzioni per convertire una stringa in minuscolo su Arduino. Ad esempio, puoi utilizzare la libreria `StringCaseConvert` che include funzioni per convertire una stringa in diverse forme, inclusi il minuscolo. Puoi esplorare le diverse opzioni per trovare quella più adatta al tuo progetto.

Inoltre, è importante sottolineare che non tutti i caratteri alfanumerici possono essere convertiti in minuscolo utilizzando questo metodo. Alcuni caratteri speciali o alfabeti diversi dall'inglese potrebbero non essere supportati. Assicurati sempre di controllare i tuoi risultati.

# Vedi anche

- [Funzione `minuscolo()` di Arduino](https://www.arduino.cc/reference/it/language/variables/data-types/string/functions/tolowercase/)
- [Libreria `StringCaseConvert` per Arduino](https://www.arduino.cc/reference/en/libraries/stringcaseconvert/)
- [Come manipolare le stringhe su Arduino](https://www.arduino.cc/en/tutorial/stringobject)