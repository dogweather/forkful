---
title:    "Bash: Confrontare due date"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Perché

In questo articolo parleremo di come comparare due date utilizzando il linguaggio di scripting Bash. La capacità di confrontare due date è essenziale per molti compiti di automazione e ci consente di eseguire diverse azioni in base al risultato del confronto. Continua a leggere per scoprire come fare!

# Come

Per iniziare, dobbiamo assicurarci di avere tutte le informazioni necessarie per eseguire il confronto. Dovremo specificare le due date che vogliamo comparare e in quale formato sono presenti. Ad esempio, la data può essere nel formato "giorno/mese/anno" o "anno-mese-giorno". Una volta che abbiamo queste informazioni, possiamo utilizzare il comando `date` con l'opzione `-d` per convertire le date in un formato facilmente confrontabile.

```
Bash
date -d "01/01/2020" +"%Y%m%d"       # Convertiamo la prima data in formato "anno-mese-giorno"
date -d "2020-01-01" +"%Y%m%d"       # Convertiamo la seconda data in formato "anno-mese-giorno"

# Output:
# 20200101
# 20200101
```

Ora che abbiamo le due date nel formato desiderato, possiamo utilizzare il tradizionale operatore di confronto `=` per verificare se le due date sono uguali o `>` e `<` per confrontare quale data è successiva all'altra.

```
Bash
if [ "20200101" = "20200101" ]; then
    echo "Le due date sono uguali!"
fi

if [ "20200101" > "20191231" ]; then
    echo "La seconda data è successiva alla prima!"
fi

# Output:
# Le due date sono uguali!
# La seconda data è successiva alla prima!
```

# Deep Dive

Oltre ai semplici confronti, Bash offre anche la possibilità di aggiungere alcune logiche per i casi in cui le date siano uguali o diverse. Ad esempio, possiamo utilizzare l'operatore `!=` per verificare se le due date sono diverse o utilizzare l'operatore `<=` e `>=` per includere la data attuale in un intervallo di date. Possiamo anche eseguire azioni diverse se le date non sono all'interno del nostro intervallo desiderato.

```
Bash
if [ "20200101" != "20200101" ]; then
    echo "Le due date sono diverse!"
fi

if [ "20200101" <= "20200131" ]; then
    echo "La data rientra nell'intervallo di gennaio!"
fi

if [ "20200101" >= "20191201" ] && [ "20200101" <= "20200331" ]; then
    echo "La data rientra nell'intervallo di inverno!"
else
    echo "La data non rientra in nessun intervallo specificato."
fi

# Output:
# La data rientra nell'intervallo di gennaio!
# La data rientra nell'intervallo di inverno!
```

Ora che conosci le basi per comparare due date in Bash, puoi utilizzare questa conoscenza per automatizzare ulteriori compiti e ottenere maggiore efficienza nel tuo flusso di lavoro.

# Vedi anche

- [How to Compare Dates in Bash](https://linuxize.com/post/how-to-compare-dates-in-bash/)
- [Bash Date and Time](https://www.tutorialspoint.com/unix_commands/bash_date.htm)
- [Working with Dates in Bash](https://www.baeldung.com/linux/dates-bash)