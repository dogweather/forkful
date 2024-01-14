---
title:                "Java: Maiuscolizzare una stringa."
simple_title:         "Maiuscolizzare una stringa."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Spesso, nel processo di sviluppo di un software, ci troviamo a dover manipolare delle stringhe di testo. Una delle operazioni più comuni che potremmo voler fare su queste stringhe è quella di capitalizzarle, ovvero convertire ogni lettera della stringa in maiuscolo. Ci possono essere diversi motivi per cui vogliamo fare ciò, ad esempio mostrare un testo in maiuscolo per evidenziarlo, o ottenere delle corrispondenze più precise in una ricerca di testo.

## Come Fare

In Java, ci sono diverse opzioni per capitalizzare una stringa. Una delle più semplici è utilizzare il metodo `toUpperCase()` della classe `String`. Vediamo un esempio pratico:

```Java
String nome = "maria";
String nomeInMaiuscolo = nome.toUpperCase();

System.out.println(nomeInMaiuscolo); // output: MARIA
```

Come vedete, dopo aver chiamato il metodo `toUpperCase()` sulla stringa `nome`, il suo valore viene modificato e la nuova stringa in maiuscolo viene salvata nella variabile `nomeInMaiuscolo`. Ovviamente, questa modifica è possibile solo perché le stringhe in Java sono immutabili, quindi ogni volta che viene effettuata un'operazione su di esse, viene creata una nuova stringa invece di modificare quella esistente.

Un'altra opzione è utilizzare la classe `StringBuilder`, che consente di modificare una stringa senza crearne una nuova ogni volta:

```Java
StringBuilder nome = new StringBuilder("maria");
nome.setCharAt(0, Character.toUpperCase(nome.charAt(0)));

System.out.println(nome.toString()); // output: Maria
```

In questo caso, utilizziamo il metodo `setCharAt()` della classe `StringBuilder` per impostare il primo carattere della stringa in maiuscolo. Dopo di che, chiamiamo il metodo `toString()` per ottenere la stringa finale.

## Approfondimenti

Esistono molte altre opzioni per capitalizzare una stringa in Java, come ad esempio l'utilizzo di espressioni regolari o libraries esterne come Apache Commons Text. Inoltre, è importante tenere in considerazione il caso delle stringhe composte da più parole, dove dobbiamo assicurarci che solo la prima lettera di ogni parola sia maiuscola. Questo può essere fatto tramite una delle tecniche sopra descritte, oppure utilizzando il metodo `split()` per dividere la stringa in basi ad uno spazio e successivamente manipolare ogni parola.

## Vedi Anche

- [Java String class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java StringBuilder class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Apache Commons Text library](https://commons.apache.org/proper/commons-text/)