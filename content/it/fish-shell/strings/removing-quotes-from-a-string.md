---
date: 2024-01-26 03:38:44.253380-07:00
description: "Come fare: Fish ha delle funzionalit\xE0 integrate magiche per questo\
  \ tipo di compito. Usa la funzione `string` senza sudare. Dai un'occhiata a questi\u2026"
lastmod: '2024-03-13T22:44:43.843944-06:00'
model: gpt-4-0125-preview
summary: "Fish ha delle funzionalit\xE0 integrate magiche per questo tipo di compito."
title: Rimuovere le virgolette da una stringa
weight: 9
---

## Come fare:
Fish ha delle funzionalità integrate magiche per questo tipo di compito. Usa la funzione `string` senza sudare. Dai un'occhiata a questi incantesimi:

```fish
# Esempio con virgolette singole
set quoted "'Ciao, Mondo!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Output: Ciao, Mondo!

# Stesso procedimento con virgolette doppie
set double_quoted "\"Ciao, Universo!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Output: Ciao, Universo!
```

## Approfondimento
Nell'era della linea di comando della pietra, avresti lottato con `sed` o `awk` per rimuovere le virgolette; un vero groviglio di backslash e flag criptici. La funzione `string` di Fish appartiene a un'era più recente, rendendo il codice più pulito e intuitivo.

Le alternative in altre shell potrebbero ancora affidarsi a questi vecchi strumenti o potrebbero utilizzare i loro metodi integrati come l'espansione dei parametri di bash o i modificatori di zsh.

La funzione `string` va oltre il taglio delle virgolette. È un coltellino svizzero per le operazioni sulle stringhe in Fish. Con `string`, puoi affettare, dividere, unire o persino fare match con espressioni regolari direttamente nel tuo terminale.

## Vedi Anche
Approfondisci `string` con l'aiuto della documentazione ufficiale:
- [Documentazione String di Fish Shell](https://fishshell.com/docs/current/commands.html#string)

Per nostalgia o quando si scrivono script con shell più tradizionali, controlla:
- [Guida a Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [Espansione dei Parametri di Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
