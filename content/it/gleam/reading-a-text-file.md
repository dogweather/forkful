---
title:    "Gleam: Lettura di un file di testo"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Perché
Stai cercando un modo semplice e veloce per leggere un file di testo nel tuo programma Gleam? Allora sei nel posto giusto! In questo articolo, scoprirai come leggere facilmente i contenuti di un file di testo utilizzando il linguaggio di programmazione Gleam. Che tu sia un principiante o un esperto, questi esempi ti saranno utili per comprendere meglio le funzionalità di lettura dei file di testo di Gleam.

# Come fare
Per leggere un file di testo in Gleam, è necessario importare il modulo ```gleam/io```. Successivamente, è possibile utilizzare la funzione ```Open.read``` per aprire il file di testo e leggerne i contenuti. L'esempio seguente mostra come leggere un file di testo e stamparne il contenuto su console:

```Gleam
import gleam/io

fn read_file(path: String) {
  Open.read(path, .StandardIO)
  |> expect("Errore durante l'apertura del file")
  |> gleam/io::read
  |> expect("Errore durante la lettura del contenuto del file")
}

fn main() {
  let content = read_file("esempio.txt")
  io::println(content)
}
```

L'output di questo codice sarà il seguente:

```Gleam
Ciao a tutti!
Benvenuti nel mondo di Gleam!
```

# Approfondimento
La funzione ```Open.read``` può accettare un secondo argomento opzionale che indica il codec da utilizzare per leggere il file di testo. Se il file di testo contiene caratteri UTF-8, puoi utilizzare il codec ```gleam/unicode``` per leggerlo correttamente. Inoltre, puoi specificare una terza opzione per indicare il numero massimo di byte da leggere dal file. Ad esempio, se vuoi leggere solo i primi 100 byte del file, puoi impostare l'opzione a ```100```. Questo può essere utile per file di grandi dimensioni in cui non hai bisogno di leggere tutti i contenuti.

# Vedi anche
- [Documentazione ufficiale di Gleam su lettura di file di testo](https://gleam.run/book/tour/file-io.html#working-with-text-files)
- [Ulteriori informazioni sul modulo ```gleam/io```](https://gleam.run/docs/stdlib/io.html)
- [Esempio di lettura di file di testo in Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/file_read_async/src/file_read_async.gleam)