---
title:                "Fish Shell: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

La programmazione con Fish Shell e YAML può sembrare intimidatoria, ma in realtà è un'ottima abilità da imparare per semplificare e automatizzare alcune attività. In questo post, esploreremo il motivo per cui dovresti considerare di lavorare con YAML e come farlo.

## Come fare

Per utilizzare YAML con Fish Shell, è necessario installare il modulo "json_yml". Una volta installato, è possibile utilizzare la funzione "json2yaml" per convertire i dati in formato YAML.

```Fish Shell
set data '{ "nome": "Marco", "cognome": "Rossi", "età": 30 }'
set yaml (json2yaml $data)
echo $yaml
```

Questo codice converte il nostro oggetto in formato JSON in un formato YAML leggibile e lo stampa a schermo.

```
nome: Marco
cognome: Rossi
età: 30
```

Oltre alla conversione, è anche possibile leggere e scrivere file YAML utilizzando i comandi standard di Fish Shell. Ad esempio, per leggere un file YAML chiamato "config.yml" e impostare alcune variabili basate su di esso, possiamo utilizzare il seguente codice:

```Fish Shell
set -q options # Verifica se la variabile "options" è già stata impostata
if not $options # Se la variabile non è impostata, procediamo a leggere il file YAML e impostarla
    set -q path # Verifica se la variabile "path" è già stata impostata
    if not $path # Se la variabile non è impostata, impostala al percorso corrente
        set path .
    end
    set -q config # Verifica se il file YAML è già stato letto
    if not $config # Se il file non è stato letto, leggilo e impostalo come variabile
        set -l config (yq r -P $path/config.yml -j)
    end

    set options $config.options
end
```

Una volta impostata, la variabile "options" conterrà un oggetto con tutti i dati del file YAML, rendendolo facilmente accessibile per ulteriori operazioni di programmazione.

## Approfondimento

Per coloro che desiderano approfondire i loro studi su YAML, ci sono molte risorse disponibili online. Alcune fonti consigliate includono la documentazione ufficiale di Fish Shell, il modulo "json_yml" su GitHub e tutorial sulle basi di YAML per programmatori.

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/)
- [Modulo "json_yml" su GitHub](https://github.com/IlanCosman/fish-json_yml)
- [Tutorial sulle basi di YAML per programmatori](https://riptutorial.com/yaml)

Grazie per aver letto! Speriamo che questo post ti abbia fornito una comprensione di base di come lavorare con YAML utilizzando Fish Shell. Buona programmazione!