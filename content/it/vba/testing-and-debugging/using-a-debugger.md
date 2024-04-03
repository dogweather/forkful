---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:27.706751-07:00
description: "In VBA, il debugger \xE8 integrato nell'Editor di Visual Basic (VBE).\
  \ Ecco come puoi sfruttarlo: 1. **Impostazione dei punti di interruzione**: Fai\
  \ clic nel\u2026"
lastmod: '2024-03-13T22:44:43.268075-06:00'
model: gpt-4-0125-preview
summary: "In VBA, il debugger \xE8 integrato nell'Editor di Visual Basic (VBE)."
title: Utilizzo di un debugger
weight: 35
---

## Come fare:
In VBA, il debugger è integrato nell'Editor di Visual Basic (VBE). Ecco come puoi sfruttarlo:

1. **Impostazione dei punti di interruzione**: Fai clic nel margine a sinistra accanto alla linea di codice che ti interessa, o posiziona il tuo cursore sulla linea e premi F9. Questo dice a VBA di interrompere l'esecuzione quando raggiunge questo punto.

    ```vb
    Sub DebugExample()
        Dim counter As Integer
        For counter = 1 To 5
            Debug.Print counter ' Imposta qui il punto di interruzione
        Next counter
    End Sub
    ```

    Quando il codice viene eseguito, si fermerà alla linea `Debug.Print counter`, permettendoti di ispezionare i valori delle variabili.

2. **Esegui fino a cursore (F8)**: Con questo comando, esegui il tuo codice un'istruzione alla volta, entrando in qualsiasi procedura chiamata. È utile per tracciare come il tuo codice e le funzioni interagiscono.

3. **Finestra di controllo**: Usa la Finestra di controllo per monitorare i valori delle variabili o delle espressioni. Se una variabile non è nell'ambito, la Finestra di controllo lo indicherà. Clicca con il tasto destro su una variabile > Aggiungi controllo.

4. **Finestra Immediata (Ctrl+G)**: Questa finestra è particolarmente utile per testare espressioni o modificare i valori delle variabili durante il debug. Digita `?nomeVariabile` per stampare il valore corrente di una variabile, o assegna un nuovo valore con `nomeVariabile = nuovoValore`.

    ```vb
    ' Nella Finestra Immediata
    ?counter ' Stampa il valore corrente di counter
    counter = 3 ' Imposta il valore di counter a 3
    ```

5. **Esempio di output**:

    Quando raggiungi il punto di interruzione ed esegui linea per linea usando F8, la Finestra Immediata potrebbe visualizzare qualcosa di simile:

    ```
    counter = 1
    counter = 2
    counter = 3
    ```

    Qui, abbiamo interrogato manualmente la variabile `counter` dopo ogni iterazione.

## Approfondimento:
Il debugger in VBA, sebbene robusto, fa parte di una tradizione più ampia di strumenti di debug nei linguaggi di programmazione, evolvendosi notevolmente dai suoi primi predecessori. Introdotti con le prime versioni di VBA, miravano a fornire agli sviluppatori un insieme di strumenti semplice ma potente per l'ispezione e la correzione del codice. Nel tempo, i miglioramenti hanno incluso punti di interruzione condizionali, capacità di controllo migliorate e l'integrazione con l'interfaccia di Excel per un'ispezione dei dati più intuitiva.

Tuttavia, rispetto agli ambienti di sviluppo integrato (IDE) moderni come Visual Studio o Eclipse, gli strumenti di debugging di VBA possono sembrare basilari. Questi IDE moderni offrono funzionalità più sofisticate come l'ispezione delle variabili in tempo reale, punti di interruzione avanzati e framework di test unitari integrati. Mentre queste alternative forniscono esperienze di debug più complete, la semplicità e la direttezza del debugger di VBA rimangono ben adattate al contesto specifico dell'automazione e della creazione di script all'interno delle applicazioni di Microsoft Office.

Per i programmatori abituati a questi ambienti moderni, l'adattamento agli strumenti di debug di VBA potrebbe richiedere un cambiamento di approccio. Eppure, i principi fondamentali dell'ispezione delle variabili, dell'esecuzione passo passo del codice e dell'osservazione del comportamento in tempo di esecuzione sono universali. Con la pratica, il debugger di VBA diventa uno strumento indispensabile per garantire che i tuoi script di automazione funzionino senza intoppi all'interno dell'ecosistema di Office.
