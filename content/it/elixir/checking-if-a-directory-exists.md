---
title:                "Elixir: Verificare se una cartella esiste"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Controllare l'esistenza di una directory è un'operazione comune quando si lavora con file e cartelle nel tuo programma Elixir. Può essere utile per assicurarsi di avere tutti i file necessari prima di eseguire un'azione o per gestire errori in modo elegante.

## Come Fare
Per prima cosa, è necessario importare il modulo `:file` utilizzando `import File` all'inizio del tuo codice. Quindi, puoi utilizzare la funzione `exists?/1` fornendo il percorso della directory come argomento. Ad esempio:
```Elixir
import File
if exists?("my_directory") do
  IO.puts "La directory esiste!"
else
  IO.puts "La directory non esiste."
end
```
Se la directory specificata esiste, il codice all'interno del primo ramo dell'if verrà eseguito, altrimenti verrà eseguito il secondo ramo.

## Approfondimento
La funzione `exists?/1` è molto utile, ma ci sono alcune cose da tenere a mente quando si utilizza. Innanzitutto, è importante notare che la funzione restituirà `true` anche se il percorso fornito punta a un file e non a una directory. Ad esempio, `exists?("myfile.txt")` restituirà `true` anche se `myfile.txt` è un file e non una directory. Inoltre, la funzione non esegue alcun controllo sui permessi di accesso alla directory. Se non si hanno i diritti di accesso, la funzione restituirà comunque `false`.

## Vedi Anche
- Documentazione su `File.exists?/1`: https://hexdocs.pm/elixir/File.html#exists?/1
- Altro su gestione dei file in Elixir: https://elixir-lang.org/getting-started/io-and-the-file-system.html