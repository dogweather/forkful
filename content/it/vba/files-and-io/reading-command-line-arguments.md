---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:24.044188-07:00
description: "Leggere gli argomenti della riga di comando in Visual Basic for Applications\
  \ (VBA) comporta l'accesso ai parametri passati al tuo programma\u2026"
lastmod: '2024-03-13T22:44:43.280436-06:00'
model: gpt-4-0125-preview
summary: Leggere gli argomenti della riga di comando in Visual Basic for Applications
  (VBA) comporta l'accesso ai parametri passati al tuo programma all'esecuzione.
title: Leggere gli argomenti della riga di comando
weight: 23
---

## Cosa & Perché?

Leggere gli argomenti della riga di comando in Visual Basic for Applications (VBA) comporta l'accesso ai parametri passati al tuo programma all'esecuzione. Questa tecnica è spesso usata per influenzare il comportamento o l'output di un programma senza la necessità di interazione da parte dell'utente, rendendo l'automazione e lo scripting compiti significativamente più semplici e versatili.

## Come fare:

A differenza di ambienti di programmazione più diretti, VBA non ha una funzionalità integrata per leggere direttamente gli argomenti della riga di comando in senso convenzionale perché è principalmente progettato per essere incorporato all'interno delle applicazioni Microsoft Office. Tuttavia, con un po' di creatività, possiamo usare Windows Script Host (WSH) o chiamare API esterne per ottenere una funzionalità simile. Ecco una soluzione pratica usando WSH:

1. **Crea uno script VBScript per Passare Argomenti a VBA:**

   Innanzitutto, scrivi un file VBScript (*yourScript.vbs*) che avvia la tua applicazione VBA (ad es. una macro di Excel) e passa gli argomenti della riga di comando:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Accedere agli Argomenti in VBA:**

   Nella tua applicazione VBA (*YourMacroWorkbook.xlsm*), modifica o crea la macro (*YourMacroName*) per accettare parametri:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Argomento 1: " & arg1 & " Argomento 2: " & arg2
End Sub
```

3. **Esegui il Tuo Script:**

   Esegui lo script VBScript dalla riga di comando, passando gli argomenti come necessario:

```shell
cscript yourScript.vbs "Hello" "World"
```

   Questo dovrebbe comportare l'esecuzione della tua macro VBA con gli argomenti "Hello" e "World", visualizzandoli in una finestra di messaggio.

## Approfondimento:

Nel contesto storico, VBA è stato concepito per estendere le capacità delle applicazioni Microsoft Office, non come ambiente di programmazione autonomo. Di conseguenza, l'interazione diretta con la riga di comando è al di fuori del suo ambito primario, il che spiega la mancanza di supporto integrato per la lettura degli argomenti della riga di comando.

Il metodo delineato sopra, sebbene efficace, è più una soluzione alternativa che una soluzione nativa, sfruttando lo scripting esterno per colmare il divario. Questo approccio può introdurre complessità e potenziali preoccupazioni per la sicurezza, poiché richiede l'abilitazione delle macro e, potenzialmente, l'abbassamento delle impostazioni di sicurezza per l'esecuzione.

Per compiti fortemente dipendenti dagli argomenti della riga di comando o che necessitano di un'integrazione più fluida con il sistema operativo Windows, altri linguaggi di programmazione come PowerShell o Python potrebbero offrire soluzioni più robuste e sicure. Queste alternative forniscono supporto diretto per gli argomenti della riga di comando e sono più adatte per applicazioni o script autonomi che richiedono un input esterno per modificare dinamicamente il loro comportamento.
