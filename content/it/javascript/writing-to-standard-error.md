---
title:    "Javascript: Scrivere su errori standard."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché

Spesso, quando scriviamo codice in Javascript, ci concentriamo sulle funzionalità del nostro programma e ci dimentichiamo di gestire gli errori che potrebbero verificarsi. Tuttavia, scrivere correttamente gli errori alla standard error può aiutare a debuggare e a individuare eventuali problemi nel nostro codice.

## Come farlo

Per scrivere correttamente gli errori alla standard error, è possibile utilizzare il metodo `console.error()`, fornito dalla libreria `console` di Javascript. Questo metodo accetta una serie di argomenti che verranno visualizzati come messaggio di errore sulla console.

```
Javascript
console.error("Errore! Variabile non definita.", undefinedVariable) 
```

Questo codice produrrà un output come questo sulla console:

```
Errore! Variabile non definita. undefined
```

In questo modo, si può identificare facilmente la causa dell'errore e lavorare per risolverlo. Si può anche utilizzare il metodo `console.warn()` per emettere un messaggio di avvertimento sulla standard error.

## Approfondimento

Scrivere correttamente gli errori alla standard error è particolarmente utile quando si gestiscono promesse in Javascript. Utilizzando il costrutto `catch` in combinazione con il metodo `console.error()`, possiamo gestire gli errori in modo più efficace e fornire un feedback migliore agli utenti del nostro programma.

```
Javascript
fetch('url_del_web_service')
    .then(response => response.json())
    .then(data => {
        // manipolazioni dei dati
    })
    .catch(error => console.error("Errore durante il fetch dei dati:", error))
```

In questo caso, se il fetch dei dati dovesse fallire, il messaggio di errore sarà visualizzato sulla standard error, aiutando a identificare il problema e a gestirlo correttamente.

## Vedi anche

- [Documentazione ufficiale di `console` in Javascript](https://developer.mozilla.org/it/docs/Web/API/Console)
- [Gestione degli errori in Javascript con `try...catch`](https://www.w3schools.com/js/js_errors.asp)