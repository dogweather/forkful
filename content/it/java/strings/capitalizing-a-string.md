---
title:                "Capitalizzare una stringa"
aliases:
- /it/java/capitalizing-a-string/
date:                  2024-02-03T19:05:41.241533-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizzare una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Capitalizzare una stringa implica modificare la prima lettera di ogni parola nella stringa in maiuscolo, assicurandosi che il resto rimanga in minuscolo. Questo compito comune di manipolazione delle stringhe è utile per formattare il testo nelle applicazioni, come preparare nomi utente o titoli per la visualizzazione secondo convenzioni o correttezza grammaticale.

## Come fare:
La libreria standard di Java non fornisce un metodo diretto per capitalizzare intere stringhe in un'unica soluzione, ma è possibile ottenere questo risultato combinando metodi integrati. Per esigenze più sofisticate, librerie di terze parti come Apache Commons Lang offrono soluzioni semplici.

### Utilizzando i Metodi Integrati di Java
Per capitalizzare una stringa senza librerie esterne, è possibile dividere la stringa in parole, capitalizzare la prima lettera di ciascuna e poi unirle nuovamente. Ecco un approccio semplice:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "ciao, mondo!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Outputs: "Ciao, Mondo!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean trovato = false;
        for (int i = 0; i < chars.length; i++) {
            if (!trovato && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                trovato = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                trovato = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

Questo frammento di codice converte l'intera stringa in minuscolo, quindi itera attraverso ogni carattere, capitalizzando la prima lettera di ogni parola. Considera spazi, punti e apostrofi come separatori di parole.

### Utilizzando Apache Commons Lang

La libreria Apache Commons Lang fornisce una soluzione più elegante con il metodo `WordUtils.capitalizeFully()`, che gestisce vari casi limite e delimitatori per te:

```java
// Aggiungi dipendenza: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "ciao, mondo!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Outputs: "Ciao, Mondo!"
    }
}
```

Per utilizzare questo metodo, sarà necessario aggiungere la libreria Apache Commons Lang al proprio progetto. Questo metodo della libreria non solo capitalizza la prima lettera di ogni parola, ma converte anche il resto delle lettere di ogni parola in minuscolo, garantendo un modello di capitalizzazione coerente in tutta la stringa.
