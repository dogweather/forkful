---
title:                "Elm: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

### Perché
Creare un file temporaneo può essere utile per gestire dati temporanei o per eseguire operazioni di scrittura senza influire sui file permanenti.

### Come fare
Per creare un file temporaneo in Elm, dobbiamo utilizzare la funzione `File.temp` fornita dalla libreria `elm/file`. Questa funzione accetta tre argomenti: 
- Un percorso (path) opzionale, che specifica la directory in cui vogliamo creare il file temporaneo. Se non viene fornito alcun percorso, il file verrà creato nella directory di lavoro corrente.
- Un prefisso opzionale, che non è altro che il primo segmento di nome del file temporaneo che verrà creato. Se viene omesso, il prefisso predefinito sarà "temp",
- Un suffisso opzionale, che è invece l'ultima parte del nome del file temporaneo. Se omesso, il suffisso predefinito sarà una stringa vuota.

Il seguente codice mostra come utilizzare la funzione `File.temp` per creare un file temporaneo nella directory di lavoro corrente con il prefisso "elm_" e il suffisso ".tmp":

```Elm
import File exposing (temp)

main =
  temp "elm_" ".tmp"
    |> Result.map (\filePath -> "File temporaneo creato: " ++ filePath)
    |> Result.withDefault "Errore!"
```

L'output del codice sarà una stringa contenente il percorso del file temporaneo appena creato. Se l'operazione non riesce, la stringa di output sarà "Errore!".

### Approfondimento
Creare un file temporaneo può essere una soluzione veloce e facile per gestire dati temporanei, ma è importante considerare alcune cose. Prima di tutto, i file temporanei vengono eliminati automaticamente al termine del programma eseguito. Inoltre, se si vuole utilizzare il file temporaneo al di fuori della funzione in cui è stato creato, si dovrà passare il percorso del file come argomento alla funzione successiva.

### Vedi anche
- Documentazione ufficiale della funzione `File.temp`: https://package.elm-lang.org/packages/elm/file/latest/File#temp
- Altro tutorial su come creare un file temporaneo in Elm: https://dev.to/ifazio/how-to-create-temporary-files-in-elm-3f94