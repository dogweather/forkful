---
title:                "Utilizzo di un debugger"
date:                  2024-01-26T04:08:50.396923-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un debugger"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/using-a-debugger.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
"Usare un debugger" significa esaminare passo dopo passo il proprio codice Python per scoprire bug e comprendere il comportamento. Lo facciamo perché è decisamente più facile che cercare di indovinare dove sono andate storte le cose, e ci risparmia ore di purgatorio con gli statement di stampa.

## Come fare:
Vediamo come utilizzare `pdb`, il debugger integrato di Python. Immagina un file, `buggy.py`, con un bug subdolo:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Eseguendo questo script, ti aspetti `8`, ma genera solo un errore di sintassi. È il momento del debugger!

Nel tuo terminale, esegui:
```bash
python -m pdb buggy.py
```

Entrerai nel debugger, e apparirà così:
```Python
> /percorso_del_file/buggy.py(1)<module>()
-> def add_one(number):
```

Usa `l(ist)` per vedere più codice, `n(ext)` per passare alla prossima riga, o `c(ontinue)` per continuare l'esecuzione dello script. Quando incontri l'errore, `pdb` si fermerà e ti permetterà di ispezionare.

Dopo aver corretto `number ++ 1` in `number + 1`, riavvia il debugger per testare la correzione.
Ricorda, gli amici non lasciano programmare gli amici senza una rete. Detto questo, basta.

## Approfondimento
Nell'Età Oscura della programmazione (alias prima che gli ambienti di sviluppo integrati, o IDE, fossero diffusi ovunque), i debugger erano spesso strumenti autonomi che si utilizzavano al di fuori dell'editor di testo. Venivano in soccorso permettendo ai programmatori di ispezionare lo stato del loro software in vari punti di esecuzione.

Allo stato attuale, nel 2023, il `pdb` di Python non è l'unico strumento disponibile. Le persone potrebbero usare IDE come PyCharm o Visual Studio Code, che hanno i loro debugger integrati e sofisticati. Questi aggiungono funzionalità utili come i breakpoint che si possono impostare con un clic, anziché digitare comandi criptici.

Poi c’è `ipdb`, un pacchetto installabile tramite pip che porta la bontà di `IPython` nel debugging. È come `pdb` sotto steroidi, con completamento automatico delle tabulazioni e evidenziazione della sintassi.

I debugger variano anche nella loro implementazione. Alcuni entrano a fondo nel dettaglio dell'esecuzione del programma a livello di macchina o di byte code. Altri, come molti debugger di linguaggi ad alto livello, eseguono il codice in un ambiente speciale che monitora lo stato delle variabili e controlla il flusso di esecuzione.

## Vedi Anche
Per il quadro completo sul debugger di Python, consulta:
- La documentazione di `pdb`: https://docs.python.org/3/library/pdb.html

Se sei curioso delle alternative, questi link ti saranno utili:
- Repository e guida all'uso di `ipdb`: https://github.com/gotcha/ipdb
- Debugging con Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- Funzionalità di debugging di PyCharm: https://www.jetbrains.com/help/pycharm/debugging-code.html

Buona caccia ai bug!
