---
title:                "Bash: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Hai mai sentito parlare di YAML e ti chiedi perché dovresti iniziare a lavorarci? Ti spiegherò in poche parole perché YAML può essere un'ottima scelta per i tuoi progetti di programmazione in Bash.

## Come fare

Iniziamo con il più importante: come si lavora con YAML in Bash? La prima cosa da fare è installare il pacchetto "yq" utilizzando il gestore dei pacchetti di Bash. Una volta installato, puoi aprire un file YAML utilizzando il comando "yq e". All'interno del tuo script Bash, puoi leggere i file YAML utilizzando la funzione "yq r" e modificare i dati utilizzando "yq w".

```Bash
# Esempio di lettura di un file YAML e stampa dei dati
yq r file.yaml

# Esempio di scrittura su un file YAML
yq w -i file.yaml nome "Marco"

# Esempio di eliminazione di un elemento dal file
yq w -i file.yaml – ripeti "[remuovi() | . età > 18]"
```

## Approfondimento

Ora che sai come utilizzare YAML in Bash, vediamo alcune funzionalità più avanzate. Per accedere ai dati all'interno di documenti YAML nidificati, puoi utilizzare la funzione "yq r --printMode v path/to/key file.yaml". Puoi anche utilizzare le funzioni "select()" e "map()" per filtrare e mappare i dati in modo più efficiente.

```Bash
# Esempio di accesso ai dati nidificati
yq r --printMode v .nome.yq file.yaml

# Esempio di filtraggio dei dati
yq r --printMode v .[] | select(.età > 18) file.yaml

# Esempio di mappatura dei dati
yq r --printMode v .[] | map({nome, età}) file.yaml
```

## Vedi anche

- [Documentazione ufficiale yq](https://github.com/kislyuk/yq)
- [Esempi di utilizzo di yq in Bash](https://www.baeldung.com/linux/yq-command-bash)
- [Tutorial sull'uso di YAML in Bash](https://linuxize.com/post/how-to-use-yaml-in-bash-scripts-with-yq/)