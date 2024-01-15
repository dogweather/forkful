---
title:                "Lavorare con json"
html_title:           "Bash: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Sei un appassionato di Bash e vuoi espandere le tue conoscenze incorporando dati JSON nei tuoi script? Oppure ti trovi spesso a dover manipolare file JSON e vuoi imparare a farlo direttamente da riga di comando? In entrambi i casi, questo articolo è quello che fa per te!

## Come fare

Per lavorare con dati JSON in Bash, ci sono due strumenti principali che puoi utilizzare: `jq` e `python`. Vediamo un un esempio per ognuno.

##### Utilizzando `jq`

```Bash
# Esempio di input JSON
input='{"nome": "Marco", "età": 25, "professione": "programmatore"}'

# Estrai il valore della chiave "nome"
nome=$(echo $input | jq '.nome')
echo $nome # Output: "Marco"

# Modifica il valore della chiave "età"
input=$(echo $input | jq '.età = 26')
echo $input # Output: {"nome": "Marco", "età": 26, "professione": "programmatore"}
```

##### Utilizzando `python`

```Bash
# Esempio di input JSON
input='{"nome": "Maria", "età": 30, "professione": "designer"}'

# Estrai il valore della chiave "età"
# Utilizzando l'opzione -c ficchiamo il codice Python nel comando python
età=$(python -c "import json; print(json.loads('$input')['età'])")
echo $età # Output: 30

# Modifica il valore della chiave "nome"
novo_nome="Giulia"
input=$(python -c "import json; x=json.loads('$input'); x['nome']='$novo_nome'; print(json.dumps(x))")
echo $input # Output: {"nome": "Giulia", "età": 30, "professione": "designer"}
```

## Approfondimento

Come puoi vedere dagli esempi, utilizzare `jq` è più semplice e diretto rispetto all'utilizzo di `python` dove è necessario convertire il JSON in un oggetto Python e poi riportarlo in JSON dopo le modifiche. Tuttavia, lavorare con `python` ti offre una maggiore flessibilità e possibilità di manipolazione dei dati. Inoltre, puoi utilizzare librerie come `pyjq` per combinare la sintassi di `jq` con la potenza di `python`.

## Vedi anche

- [Documentazione di jq](https://stedolan.github.io/jq/)
- [Documentazione di python](https://docs.python.org/3/library/json.html)
- [Libreria pyjq](https://pypi.org/project/pyjq/)