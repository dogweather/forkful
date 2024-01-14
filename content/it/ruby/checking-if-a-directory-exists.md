---
title:                "Ruby: Verifica dell'esistenza di una directory"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché controllare se una cartella esiste in Ruby

Se stai scrivendo un programma in Ruby che lavora con file e cartelle, potresti voler controllare se una certa cartella esiste prima di eseguire un'azione su di essa. Questa pratica può aiutare a evitare errori o crash del programma.

Per esempio, se stai cercando di creare una nuova cartella, potresti prima verificare se una cartella con lo stesso nome già esiste per evitare di sovrascrivere dati importanti. Inoltre, controllare l'esistenza di una cartella può aiutare a gestire eventuali errori di accesso ai file.

## Come controllare se una cartella esiste in Ruby

Controllare se una cartella esiste in Ruby è semplice e può essere fatto con il metodo `Dir.exist?` seguito dal percorso della cartella che si desidera controllare. Il seguente codice esempio mostra come utilizzare questo metodo:

```Ruby
if Dir.exist?("/Users/utente/Documenti/progetto/ruby")
  puts "La cartella esiste!"
else
  puts "La cartella non esiste."
end
```

Se la cartella `/Users/utente/Documenti/progetto/ruby` esiste, il programma stamperà "La cartella esiste!". In caso contrario, verrà stampato "La cartella non esiste.".

## Approfondimento sul controllo delle cartelle in Ruby

Esistono altre opzioni per controllare l'esistenza di una cartella in Ruby, come ad esempio il metodo `File.directory?` che controlla se il percorso specificato è una cartella. Inoltre, è possibile utilizzare il metodo `Pathname#directory?` se stai lavorando con oggetti `Pathname` per i percorsi dei file.

Un'altra cosa da tenere in considerazione durante la verifica dell'esistenza di una cartella è la differenza tra un percorso assoluto e uno relativo. Un percorso assoluto è il percorso completo della cartella a partire dalla radice del sistema operativo, mentre un percorso relativo è relativo alla posizione del proprio programma. Assicurarsi di utilizzare il percorso corretto quando si controlla l'esistenza di una cartella.

## Vedi anche

Ecco alcuni link utili per approfondire ulteriormente il controllo delle cartelle in Ruby:

- [Documentazione ufficiale di `Dir.exist?`](https://ruby-doc.org/core-3.0.0/Dir.html#method-c-exist-3F)
- [Tutorial su come gestire file e cartelle in Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Come ottenere il percorso assoluto di una cartella in Ruby](https://stackoverflow.com/questions/1643919/how-to-get-an-absolute-directory-path-in-ruby)