---
title:                "Generazione di numeri casuali"
date:                  2024-01-27T20:32:59.689853-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Generare numeri casuali in C comporta la creazione di sequenze di numeri che non presentano alcun schema discernibile, imitando il concetto di casualità. I programmatori sfruttano i numeri casuali per una miriade di scopi, inclusa la simulazione di dati, le applicazioni crittografiche e lo sviluppo di giochi, rendendolo un aspetto vitale della programmazione.

## Come fare:

Per generare numeri casuali in C, si utilizza tipicamente la funzione `rand()` trovata in `stdlib.h`. Tuttavia, è cruciale inizializzare il generatore di numeri casuali per garantire variabilità nei numeri generati attraverso diverse esecuzioni del programma. La funzione `srand()`, inizializzata con un valore, spesso il tempo corrente, facilita questo processo.

Ecco un semplice esempio di generazione di un numero casuale tra 0 e 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
	// Inizializza il generatore di numeri casuali
	srand((unsigned) time(NULL));

	// Genera un numero casuale tra 0 e 99
	int randomNumber = rand() % 100;

	// Stampa il numero casuale
	printf("Numero Casuale: %d\n", randomNumber);

	return 0;
}
```

Esempio di output:

```
Numero Casuale: 42
```

È importante notare che ogni esecuzione di questo programma produrrà un nuovo numero casuale, grazie all'inizializzazione con il tempo corrente.

## Approfondimento

Il metodo tradizionale di generazione di numeri casuali in C, utilizzando `rand()` e `srand()`, non è veramente casuale. È pseudocasuale. Questo va bene per molte applicazioni, ma non è sufficiente in situazioni che richiedono un alto grado di casualità, come nell'uso crittografico serio. La sequenza generata da `rand()` è interamente determinata dal seme fornito a `srand()`. Pertanto, se il seme è noto, la sequenza può essere prevista, riducendo la casualità.

Storicamente, la funzione `rand()` è stata criticata per la sua bassa qualità di casualità e l'intervallo limitato. Alternative moderne includono l'uso di API specifiche del dispositivo o librerie esterne che approssimano meglio la vera casualità o, nei sistemi simili a UNIX, la lettura da `/dev/random` o `/dev/urandom` per scopi crittografici.

Ad esempio, utilizzando `/dev/urandom` in C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
	FILE *fp;
	unsigned int randomNumber;

	// Apri /dev/urandom per la lettura
	fp = fopen("/dev/urandom", "r");

	// Legge un numero casuale
	fread(&randomNumber, sizeof(randomNumber), 1, fp);

	// Stampa il numero casuale
	printf("Numero Casuale: %u\n", randomNumber);

	// Chiudi il file
	fclose(fp);

	return 0;
}
```

Questo metodo legge direttamente dal pool di entropia del sistema, offrendo una qualità di casualità superiore adatta per applicazioni più sensibili. Tuttavia, questo approccio può avere problemi di portabilità tra diverse piattaforme, rendendolo meno universale dell'uso di `rand()`.

Indipendentemente dal metodo, comprendere la natura della casualità e la sua implementazione in C è fondamentale per sviluppare applicazioni efficaci, sicure e coinvolgenti.
