---
title:    "Arduino: Convertire una stringa in maiuscolo"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Quando si lavora con stringhe di testo in Arduino, ci potrebbe essere la necessità di capitalizzare una stringa. Ciò può essere utile per uniformare il testo o per facilitare la ricerca all'interno di una stringa. In questo articolo, vedremo come farlo in modo semplice e veloce.

## Come fare

Per capitalizzare una stringa in Arduino, è possibile utilizzare la funzione `toUpperCase()`. Questa funzione prende come parametro una stringa e la restituisce in maiuscolo. Ecco un esempio di codice:

```Arduino
// Dichiarazione della stringa
String str = "ciao mondo";

// Capitalizzazione della stringa
str = str.toUpperCase();

// Stampa della stringa
Serial.println(str);  // Output: CIAO MONDO
```

Come si vede dall'esempio, abbiamo dichiarato una stringa di testo, "ciao mondo", e poi abbiamo usato la funzione `toUpperCase()` per capitalizzarla. Infine, attraverso la funzione `Serial.println()`, abbiamo stampato il risultato a schermo.

## Approfondimento

Oltre alla funzione `toUpperCase()`, è possibile capitalizzare una stringa manualmente utilizzando i metodi della classe `String`. Ad esempio, si può utilizzare il metodo `charAt()` per ottenere il carattere in una posizione specifica della stringa e poi utilizzare il metodo `toUpperCase()` della classe `char` per convertire quel carattere in maiuscolo. Questo può essere fatto all'interno di un ciclo `for` per capitalizzare l'intera stringa. Ecco un esempio di codice:

```Arduino
// Dichiarazione della stringa
String str = "ciao mondo";

// Ciclo per scorrere ogni carattere della stringa
for(int i = 0; i < str.length(); i++){
  char c = str.charAt(i);  // Ottiene il carattere alla posizione i
  c = toupper(c);  // Converte il carattere in maiuscolo
  str.setCharAt(i, c);  // Sostituisce il carattere nella stringa
}

// Stampa della stringa
Serial.println(str);  // Output: CIAO MONDO
```

Come si può vedere, con questo metodo si può ottenere lo stesso risultato utilizzando solo metodi della classe `String`.

## Vedi anche

- [Documentazione ufficiale di Arduino - Funzioni String](https://www.arduino.cc/en/Reference/StringFunctions)
- [Tutorial di Arduino - Manipolazione di stringhe](https://create.arduino.cc/projecthub/feilipu/arduino-tutorial-string-manipulation-a1963b)