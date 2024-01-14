---
title:    "Arduino: Verifica se una directory esiste"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Controllare l'esistenza di una directory può essere utile per assicurarsi che il proprio programma sia in grado di gestire correttamente i file su Arduino. Ad esempio, se un programma ha bisogno di accedere a un file in una determinata directory, è importante verificare che tale directory esista prima di provare ad aprirla o leggere il file al suo interno.

## Come Fare

Per controllare l'esistenza di una directory su Arduino, è necessario utilizzare una funzione chiamata `exists()`. Questa funzione accetta come parametro una stringa contenente il percorso della directory che si desidera controllare e restituisce `true` se la directory esiste, altrimenti restituisce `false`.

```Arduino
if (exists("/my_directory")) {
  Serial.println("La directory esiste!");
} else {
  Serial.println("La directory non esiste.");
}
```

L'esempio sopra utilizza la funzione `Serial.println()` per stampare un messaggio sul monitor seriale a seconda del risultato dell'operazione di controllo. 

## Approfondimento

La funzione `exists()` in realtà utilizza due diverse funzioni interne per controllare l'esistenza della directory. La prima funzione, `open()`, tenta di aprire la directory specificata e restituisce un puntatore al file se l'operazione ha successo. Altrimenti, restituisce un puntatore nullo. La seconda funzione, `exists()`, confronta il puntatore restituito dalla funzione `open()` con il puntatore nullo per determinare se la directory esiste o meno.

È importante notare che la funzione `exists()` non crea effettivamente una nuova directory in caso di esito negativo. Per questo compito, è necessario utilizzare una funzione apposita come ad esempio `mkdir()`.

## Vedi Anche

- [documentazione ufficiale di Arduino su exists()](https://www.arduino.cc/en/Reference/Exists)
- [tutorial su come gestire file e directory su Arduino](https://www.tutorialspoint.com/arduino/arduino_file_management.htm)
- [altre funzioni utili per la gestione dei file su Arduino](https://forum.arduino.cc/index.php?topic=534431.0)
- [esempio di utilizzo di exists() su GitHub](https://github.com/greiman/SdFat/blob/master/examples/exists/exists.ino)