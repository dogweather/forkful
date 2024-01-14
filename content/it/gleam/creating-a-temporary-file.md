---
title:    "Gleam: Creazione di un file temporaneo"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Creare file temporanei è un'operazione comune nei linguaggi di programmazione, incluso Gleam. Spesso è necessario quando si lavora con dati temporanei o si vuole evitare di sovrascrivere un file esistente.

## Come fare

Ecco un esempio di codice in Gleam per creare un file temporaneo utilizzando la libreria [tempfile](https://hex.pm/packages/tempfile):

```Gleam
import tempfile

// Creare un nuovo file temporaneo
let tmpFile = tempfile.create()

// Scrivere dei dati nel file
let data = "Questo è un esempio di dati"
tmpFile.write(data)

// Chiudere il file
tmpFile.close()
```

Ecco un output di esempio una volta eseguito il codice:

> ```bash
> $ cat /tmp/tmpIOsfrs 
> Questo è un esempio di dati
> ```

## Approfondimento

Creare file temporanei può essere utile anche per scopi di sicurezza. Ad esempio, se si desidera memorizzare dati sensibili come password, è possibile creare un file temporaneo in un determinato percorso e impostare i permessi di accesso in modo che solo il programma in esecuzione possa accedervi.

Inoltre, è possibile utilizzare la libreria [os](https://hex.pm/packages/os) per gestire meglio i file temporanei, ad esempio spostandoli o eliminandoli quando non sono più necessari.

## Vedi anche

- [Documentazione ufficiale della libreria tempfile di Gleam](https://hexdocs.pm/gleam/Tempfile.html)
- [Documentazione ufficiale della libreria os di Gleam](https://hexdocs.pm/gleam/os.html)