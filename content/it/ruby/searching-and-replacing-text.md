---
title:                "Ruby: Cercare e sostituire il testo"
simple_title:         "Cercare e sostituire il testo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Una delle funzionalità più utili nella programmazione è la possibilità di cercare e sostituire testo. Questo può aiutare a correggere errori, modificare dati o semplicemente risparmiare tempo nella scrittura del codice.

## Come fare

Per cercare e sostituire testo in un programma Ruby, si può utilizzare il metodo `gsub`. Ad esempio, se si vogliono sostituire tutte le lettere "a" con la lettera "e" in una stringa, si può utilizzare il seguente codice:

```Ruby
stringa = "casa"
nuova_stringa = stringa.gsub("a", "e")
# Output: "cese"
```

Si noti che il metodo `gsub` sostituisce tutte le occorrenze della lettera specificata nella stringa. Se si vuole sostituire solo la prima occorrenza, è possibile utilizzare il metodo `sub`.

Un altro trucco utile per la ricerca e sostituzione di testo è l'utilizzo dei "regex" (espressioni regolari). Questo consente di specificare un pattern di ricerca più complesso. Ad esempio, se si vuole sostituire tutte le vocali con un asterisco, si può utilizzare il seguente codice:

```Ruby
stringa = "casa"
nuova_stringa = stringa.gsub(/[aeiou]/, "*")
# Output: "c*s*"
```

Per ulteriori informazioni sulle espressioni regolari e su come utilizzarle per la ricerca e la sostituzione di testo, si consiglia di consultare la documentazione ufficiale di Ruby.

## Approfondimento

Il metodo `gsub` può accettare anche un blocco di codice come argomento, permettendo di eseguire manipolazioni più avanzate. Ad esempio, se si volesse sostituire solo le vocali in maiuscolo con un asterisco e lasciare le vocali in minuscolo normali, si può utilizzare il seguente codice:

```Ruby
stringa = "CasA"
nuova_stringa = stringa.gsub(/[A-Z]/) { |match| match.downcase == match ? "*" : match }
# Output: "Ca*"
```

Come si può vedere, il blocco di codice prende in considerazione ogni corrispondenza trovata dall'espressione regolare e decide se sostituirla o meno in base al suo case.

## Vedi anche

- Documentazione ufficiale di Ruby su `gsub`: https://ruby-doc.org/core-2.6.1/String.html#method-i-gsub
- Guida alle espressioni regolari in Ruby: https://www.rubyguides.com/2015/06/ruby-regex/