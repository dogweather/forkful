---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:42.769200-07:00
description: "Le espressioni regolari (regex) sono sequenze di caratteri che formano\
  \ un modello di ricerca, utilizzato principalmente per la ricerca e la manipolazione\u2026"
lastmod: 2024-02-19 22:05:02.704930
model: gpt-4-0125-preview
summary: "Le espressioni regolari (regex) sono sequenze di caratteri che formano un\
  \ modello di ricerca, utilizzato principalmente per la ricerca e la manipolazione\u2026"
title: Utilizzo delle espressioni regolari
---

{{< edit_this_page >}}

## Cosa e perché?

Le espressioni regolari (regex) sono sequenze di caratteri che formano un modello di ricerca, utilizzato principalmente per la ricerca e la manipolazione di stringhe. I programmatori sfruttano le regex in PowerShell per compiti come la convalida dei dati, l'analisi e la trasformazione grazie alla sua efficienza e flessibilità nella gestione di modelli complessi.

## Come fare:

In PowerShell, puoi usare gli operatori `-match`, `-replace` e `-split`, tra gli altri, per eseguire azioni con le espressioni regolari. Esploriamo alcuni esempi:

### Usare `-match` per controllare se una stringa corrisponde a un modello
Questo operatore restituisce `$true` se il modello viene trovato all'interno della stringa, e `$false` in caso contrario.

```powershell
"hello world" -match "\w+orld"
# Output: True
```

### Estrarre le corrispondenze
Puoi estrarre il valore corrispondente accedendo alla variabile automatica `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "Numero trovato: " + $matches[0]
}
# Output: Numero trovato: 100
```

### Usare `-replace` per le sostituzioni
L'operatore `-replace` sostituisce tutte le occorrenze di un modello con una stringa di sostituzione specificata.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# Output: foo qux qux
```

### Dividere le stringhe con `-split`
Dividi una stringa in un array di sottostringhe basato su un modello regex.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# Output: The quick brown fox jumps
```

### Corrispondenza di modelli avanzata
PowerShell supporta anche operazioni regex più complesse tramite la classe `[regex]`, dandoti accesso a metodi come `Matches()`, `Replace()`, e `Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# Output: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# Output: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# Output: one two three four
```

Questi esempi mostrano la potenza e la versatilità delle espressioni regolari in PowerShell per la manipolazione dei dati e la corrispondenza di modelli. Sfruttando le regex, i programmatori possono eseguire l'elaborazione di testi complessi in modo efficiente.
