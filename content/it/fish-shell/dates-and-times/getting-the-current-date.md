---
title:                "Ottenere la data corrente"
aliases:
- /it/fish-shell/getting-the-current-date.md
date:                  2024-02-03T19:09:20.712177-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?
Ottenere la data corrente nella programmazione è un'operazione fondamentale che consente di recuperare e manipolare i dati di data e ora del sistema. Nelle attività di scripting e automazione, è essenziale per generare timestamp, pianificare attività e creare log.

## Come fare:
Fish Shell utilizza comandi esterni come `date` per ottenere la data corrente, offrendo flessibilità nella formattazione dell'output secondo necessità. Ecco come utilizzarlo:

```fish
# Mostra la data corrente nel formato predefinito
echo (date)

# Esempio di output: Wed 25 Oct 2023 15:42:03 BST
```

Per personalizzare il formato della data, puoi usare l'opzione `+` seguita dai specificatori di formato:

```fish
# Mostra la data corrente nel formato YYYY-MM-DD
echo (date "+%Y-%m-%d")

# Esempio di output: 2023-10-25
```

Per compiti più complessi, come lavorare con timestamp o eseguire aritmetica con le date, Fish Shell si affida a strumenti esterni come `date` a causa della sua natura di script. Ecco un esempio per ottenere il timestamp UNIX corrente:

```fish
# Ottieni il timestamp UNIX corrente
echo (date "+%s")

# Esempio di output: 1666710123
```

E per aggiungere un giorno alla data corrente usando `date`:

```fish
# Aggiungi un giorno alla data corrente
echo (date -d "+1 day" "+%Y-%m-%d")

# Esempio di output: 2023-10-26
```

Nota: Gli esempi utilizzano opzioni del comando `date` che funzionano con GNU coreutils. Le opzioni possono variare in altri ambienti come macOS, che utilizza di default il comando date di BSD. Fare sempre riferimento a `date --help` o alla pagina man per dettagli specifici al proprio ambiente.
