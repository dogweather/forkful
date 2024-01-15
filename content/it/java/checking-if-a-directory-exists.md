---
title:                "Verifica se una directory esiste."
html_title:           "Java: Verifica se una directory esiste."
simple_title:         "Verifica se una directory esiste."
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Potresti voler verificare l'esistenza di una directory in modo da poter gestire i file o le risorse presenti al suo interno in modo efficace.

## Come fare

Per verificare se una directory esiste in Java, puoi utilizzare il metodo `exists()` della classe `File`. Ecco un esempio di codice che mostra come farlo:

```Java
import java.io.File;

public class CheckDirectory {

    public static void main(String[] args) {
        // Specifica il percorso della directory da verificare
        String path = "C:\\Users\\Nome utente\\Documents\\Progetto";
        
        // Crea un'istanza di File
        File directory = new File(path);
        
        // Utilizza il metodo exists() per verificare l'esistenza della directory
        if (directory.exists()) {
            System.out.println("La directory esiste!");
        } else {
            System.out.println("La directory non esiste!");
        }
    }
}

// Output: La directory esiste!
```

Puoi anche utilizzare il metodo `isDirectory()` per verificare se l'oggetto `File` corrisponde a una directory anziché a un file.

## Approfondimento

Quando utilizzi il metodo `exists()` per verificare l'esistenza di una directory, è importante considerare alcuni aspetti aggiuntivi. Innanzitutto, il metodo restituirà `true` anche se la directory è vuota. Inoltre, se la directory specificata non esiste, il metodo genererà un'eccezione di tipo `SecurityException`.

Un'altra opzione per verificare l'esistenza di una directory è utilizzare il metodo `mkdir()` per creare una nuova directory e verificare il suo esito. Se il metodo restituisce `true`, significa che la directory è stata correttamente creata, mentre se restituisce `false`, significa che la directory esisteva già o che non è stato possibile crearla per qualche ragione.

## Vedi anche

- [Documentazione ufficiale di Java per la classe File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tutorial su come gestire file e directory in Java](https://www.baeldung.com/java-file-io)