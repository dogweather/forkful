---
date: 2024-01-20 17:56:06.744495-07:00
description: 'How to: - Come fare: Per leggere gli argomenti dalla linea di comando
  in Node.js, puoi usare `process.argv`. Ecco un esempio.'
lastmod: '2024-04-05T21:53:44.576211-06:00'
model: gpt-4-1106-preview
summary: ''
title: Lettura degli argomenti della riga di comando
weight: 23
---

## How to: - Come fare:
Per leggere gli argomenti dalla linea di comando in Node.js, puoi usare `process.argv`. Ecco un esempio:

```javascript
// leggi-argomenti.js
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// Per eseguire, usa:
// node leggi-argomenti.js argomento1 argomento2
```

Output esempio:

```
0: /percorso/node
1: /percorso/della/cartella/leggi-argomenti.js
2: argomento1
3: argomento2
```

## Deep Dive - Approfondimento
Node.js include `process.argv`, una proprietà dell'oggetto `process`, da quando è stato creato. Include l'interprete, il percorso dello script e poi gli argomenti. Dalla versione 11.14, abbiamo `process.argv.slice(2)` per saltare i primi due. Esistono alternative come `minimist` o `commander` per maneggiare argomenti complessi, con flags e comandi. Il parsing manuale degli argomenti è ok per piccoli script, per grandi applicazioni le librerie esterne rendono il codice più leggibile e manutenibile.

## See Also - Vedi Anche:
- Documentazione Node.js `process.argv`: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Repository GitHub `minimist`: https://github.com/substack/minimist
- Repository GitHub `commander`: https://github.com/tj/commander.js
