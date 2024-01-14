---
title:    "Python: Lettura degli argomenti della riga di comando"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché
Molti programmatori si chiedono perché dovrebbero leggere gli argomenti della riga di comando nei loro script Python. In questo blog post scopriremo come questa semplice funzione può rendere il nostro codice più flessibile e personalizzabile!

## Come fare
Per leggere gli argomenti della riga di comando in Python, possiamo utilizzare il modulo "sys". Vediamo un esempio pratico:

```Python
import sys

# Leggere un argomento dalla riga di comando
nome = sys.argv[1]
print("Ciao", nome)
```

Se eseguiamo questo script da linea di comando con l'argomento "Marco", otterremo l'output "Ciao Marco". Possiamo anche leggere più argomenti aggiungendo ulteriori elementi alla lista "sys.argv".

Possiamo anche utilizzare il modulo "argparse" che ci permette di gestire gli argomenti in modo più strutturato e professionale. Vediamo un altro esempio pratico:

```Python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--nome", help="Inserisci un nome")
parser.add_argument("--cognome", help="Inserisci un cognome")
args = parser.parse_args()

print("Buongiorno", args.nome, args.cognome)
```

Se eseguiamo questo script con i parametri "--nome=Luca" e "--cognome=Rossi", otterremo l'output "Buongiorno Luca Rossi". Questo è solo un esempio di cosa possiamo fare con il modulo "argparse". Potete approfondire ulteriormente la documentazione ufficiale per maggiori informazioni.

## Deep Dive
Ora che abbiamo visto alcuni esempi pratici, è importante capire come funzionano esattamente i moduli "sys" e "argparse" per leggere gli argomenti della riga di comando.

Il modulo "sys" ci permette di accedere alla lista "sys.argv", che è una lista di stringhe contenenti gli argomenti passati al nostro script. La prima stringa è sempre il nome del nostro script, seguita dagli eventuali argomenti.

Il modulo "argparse" invece ci fornisce una struttura più complessa per gestire gli argomenti. Possiamo definire quali argomenti accettare e definire anche delle opzioni più avanzate come argomenti posizionali, argomenti opzionali e parametri con valori predefiniti. Questo rende il nostro codice più leggibile e manutenibile.

## See Also
- Documentazione ufficiale di Python per il modulo "sys": <https://docs.python.org/3/library/sys.html>
- Documentazione ufficiale di Python per il modulo "argparse": <https://docs.python.org/3/library/argparse.html>
- Un tutorial dettagliato sull'utilizzo del modulo "argparse": <https://realpython.com/command-line-interfaces-python-argparse/>

Grazie per aver letto questo blog post! Speriamo che ora abbiate una migliore comprensione di come leggere gli argomenti della riga di comando in Python. Continuate a migliorare le vostre abilità di programmazione e a esplorare tutte le potenzialità di questo linguaggio!