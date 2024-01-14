---
title:    "TypeScript: Estrazione di sottostringhe"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe è un'operazione comune nella programmazione di TypeScript. Ciò consente ai programmatori di accedere a parti specifiche di una stringa, come una parola o una frase, per eseguire operazioni su di essa. Estrarre sottostringhe è particolarmente utile quando si lavora con dati di input forniti dagli utenti, in cui è necessario eseguire controlli o manipolazioni su una stringa di input selezionata.

## Come Fare

Per estrarre una sottostringa in TypeScript, è necessario utilizzare il metodo `substring()` su una variabile di tipo stringa. Questo metodo accetta due parametri: l'indice iniziale della sottostringa e l'indice finale desiderato. Ad esempio, supponiamo di avere una variabile di stringa chiamata `frase` che contiene "Ciao a tutti!" e vogliamo estrarre solo la parola "Ciao". Possiamo farlo utilizzando il seguente codice:

```TypeScript
let frase = "Ciao a tutti!";
let sottostringa = frase.substring(0, 4);
console.log(sottostringa); // Output: Ciao
```

Il primo parametro dell'`substring()` indica l'indice della prima lettera che vogliamo includere nella nostra sottostringa, mentre il secondo parametro indica l'indice della lettera successiva a quella che vogliamo includere. In questo caso, vogliamo iniziare dalla lettera "C" che ha un indice di 0 e terminare prima della lettera "a", che ha un indice di 4.

Alcune altre cose da tenere a mente quando si tratta di estrarre sottostringhe in TypeScript sono:

- Gli indici iniziano sempre da 0.
- Se si omette il secondo parametro, il `substring()` estrarrà tutto fino alla fine della stringa.
- Se si utilizza un numero negativo per l'indice iniziale o finale, questo verrà interpretato come una posizione dall'ultimo carattere della stringa. Ad esempio, -1 corrisponde all'ultimo carattere, -2 al penultimo e così via.

Considerando l'esempio precedente, se utilizziamo un indice finale di -7, l'output sarebbe "Ciao a".

## Approfondimento

Oltre al metodo `substring()`, TypeScript offre anche altri modi per estrarre sottostringhe da una stringa. Ad esempio:

- Il metodo `slice()` funziona allo stesso modo del `substring()` ma accetta anche valori negativi come parametri.
- Il metodo `substr()` accetta invece un indice iniziale e la lunghezza desiderata della sottostringa.
- È possibile utilizzare anche la sintassi di array per accedere a singoli caratteri di una stringa. Ad esempio, `frase[0]` accederà alla prima lettera della stringa.

Inoltre, TypeScript offre anche metodi per controllare la presenza di una sottostringa all'interno di una stringa, come `includes()`, `startsWith()` e `endsWith()`.

## Vedi anche

- [Documentazione ufficiale del metodo `substring()` di TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-substring)
- [Esempi di utilizzo del `substring()`](https://www.javatpoint.com/typescript-substring)
- [Video tutorial su come estrarre sottostringhe in TypeScript](https://www.youtube.com/watch?v=j_FnBFCi_cg)