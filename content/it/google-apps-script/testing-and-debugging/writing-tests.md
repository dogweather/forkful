---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:32.747751-07:00
description: "Scrivere test in Google Apps Script (GAS) consiste nel creare script\
  \ automatizzati per verificare il comportamento dei propri codici, assicurandosi\
  \ che\u2026"
lastmod: 2024-02-19 22:05:02.065604
model: gpt-4-0125-preview
summary: "Scrivere test in Google Apps Script (GAS) consiste nel creare script automatizzati\
  \ per verificare il comportamento dei propri codici, assicurandosi che\u2026"
title: Scrivere test
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere test in Google Apps Script (GAS) consiste nel creare script automatizzati per verificare il comportamento dei propri codici, assicurandosi che funzionino come previsto in varie condizioni. I programmatori lo fanno per individuare i bug precocemente, migliorare la qualità del codice e facilitare aggiornamenti e manutenzione più semplici.

## Come fare:

Sebbene Google Apps Script non disponga di un framework di testing integrato come alcuni altri ambienti di programmazione, è comunque possibile scrivere ed eseguire test utilizzando semplici funzioni GAS o integrando librerie di testing esterne come `QUnit`. Ecco un esempio base che utilizza una semplice funzione GAS per testare un'altra funzione nel tuo script:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Test fallito: add(2, 3) dovrebbe essere 5, ma è stato " + result);
  } else {
    Logger.log("Test superato!");
  }
}
```

Eseguire `testAdd()` registrerà "Test superato!" se la funzione `add` funziona correttamente, oppure genererà un errore se non lo fa. Per un approccio più sofisticato, integrare QUnit con Google Apps Script comporta alcuni passaggi in più ma offre un ambiente di testing potente. Una configurazione di test QUnit di esempio appare così:

1. Includere la libreria QUnit nel tuo progetto.
2. Creare un file HTML di test per eseguire i test QUnit.
3. Scrivere casi di test utilizzando la sintassi di QUnit.

Ecco un esempio che utilizza QUnit:

```javascript
// Includere QUnit collegandolo in un file HTML usato per eseguire i tuoi test

QUnit.test("Test della funzione add", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) dovrebbe restituire 5");
});
```

Per vedere i risultati, aprire il file HTML all'interno dell'Editor di Script GAS o distribuirlo come app web.

## Approfondimento

Storicamente, il testing in Google Apps Script è stato in qualche modo trascurato, probabilmente a causa delle origini della piattaforma e dei casi d'uso principali focalizzati su attività di automazione veloci e su piccola scala piuttosto che su grandi applicazioni. Di conseguenza, GAS non offre gli stessi robusti framework e strumenti di testing trovati in ambienti di programmazione più tradizionali. Tuttavia, la comunità si è adattata incorporando librerie open-source e sfruttando creativamente gli strumenti esistenti di Google.

Utilizzare librerie come QUnit rappresenta un significativo passo avanti, ma porta con sé una serie di sfide, come l'allestimento di un ambiente di testing adatto e l'apprendimento di una sintassi aggiuntiva. Tuttavia, per coloro che sono impegnati a costruire applicazioni più complesse e affidabili con GAS, lo sforzo ne vale la pena.

Alternative come l'utilizzo di semplici funzioni GAS per i test offrono facilità d'uso e integrazione con l'ambiente GAS senza dipendenze aggiuntive, ma mancano di funzionalità di testing complete e della capacità di scalare facilmente man mano che il progetto cresce. Strumenti come clasp (l'interfaccia a riga di comando di Google Apps Script) possono facilitare workflow più avanzati, inclusi i test, consentendo agli sviluppatori di codificare nel loro IDE preferito, introducendo spazio per integrarsi più agevolmente con framework di testing esterni.

In conclusione, sebbene GAS potrebbe non avere un supporto nativo per testing sofisticati subito disponibili, la sua flessibilità e gli approcci innovativi della comunità forniscono vie praticabili per garantire che i tuoi script siano robusti, affidabili e pronti per qualsiasi compito.
