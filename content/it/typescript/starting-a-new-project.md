---
title:    "TypeScript: Avviare un nuovo progetto"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Se sei uno sviluppatore alla ricerca di un nuovo progetto da iniziare, allora TypeScript potrebbe essere la scelta perfetta per te. Questo linguaggio di programmazione ti permette di scrivere codice in modo strutturato e di avere un controllo maggiore sul tuo codice, mentre allo stesso tempo ti offre tutte le funzioni avanzate di JavaScript.

## Come iniziare

Per iniziare un nuovo progetto TypeScript, devi seguire questi passaggi:

1. Installa TypeScript attraverso il gestore di pacchetti di Node.js:
```TypeScript
npm install -g typescript
```

2. Crea una cartella per il tuo nuovo progetto e naviga al suo interno:
```TypeScript
mkdir nuovo-progetto
cd nuovo-progetto
```

3. Inizializza il tuo progetto TypeScript:
```TypeScript
tsc --init
```

4. Questo creerà un file `tsconfig.json` che contiene le impostazioni del tuo progetto TypeScript.

5. Ora puoi creare il tuo primo file TypeScript con l'estensione `.ts`:
```TypeScript
// In file: saluti.ts
function saluta(nome: string) {
    console.log("Ciao, " + nome);
}

saluta("Mario"); // Output: Ciao, Mario
```

6. Puoi compilare il file TypeScript in un file JavaScript eseguendo il seguente comando:
```TypeScript
tsc saluti.ts
```

7. Avrai ora un file `saluti.js` con il seguente codice JavaScript:
```JavaScript
function saluta(nome) {
    console.log("Ciao, " + nome);
}

saluta("Mario"); // Output: Ciao, Mario
```

8. Puoi eseguire il file JavaScript ottenuto con Node.js:
```TypeScript
node saluti.js // Output: Ciao, Mario
```

## Approfondimento

Oltre a fornirti i passaggi basilari per iniziare un nuovo progetto TypeScript, è importante menzionare alcune delle sue funzionalità più avanzate.

- Tipi di dati - TypeScript ti permette di specificare i tipi di dato delle variabili, delle funzioni e dei parametri, aiutandoti a individuare più facilmente errori di tipo durante la fase di sviluppo.
- Classe - TypeScript supporta l'utilizzo delle classi, permettendoti di scrivere codice più strutturato e di organizzare il tuo progetto in modo più efficace.
- Decoratori - Questa funzionalità avanzata ti permette di applicare annotazioni o metadati alle tue classi, funzioni o proprieta per ottenere un comportamento specifico.
- Supporto per i moduli - TypeScript offre un eccellente sistema per importare ed esportare moduli e librerie, aiutandoti a gestire le dipendenze del tuo progetto.

Inoltre, TypeScript è supportato da molte popolari piattaforme e framework come Angular, React e Vue.js, rendendolo una scelta ideale per la creazione di applicazioni web.

## Vedi anche

- [Documentazione di TypeScript](https://www.typescriptlang.org/docs/)
- [Tutorial su TypeScript su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Classes)