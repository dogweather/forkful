---
title:    "Java: Convertire una stringa in minuscolo"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché 
Ci sono molte ragioni per cui si potrebbe voler convertire una stringa in lettere minuscole. Ad esempio, potrebbe essere necessario confrontare due stringhe, ma non si vuole che le lettere maiuscole influenzino il risultato o si potrebbe avere una stringa di input con diverse combinazioni di lettere maiuscole e minuscole e si vuole normalizzarle per facilitare il confronto.

## Come fare
Ci sono diversi modi per convertire una stringa in lettere minuscole in Java. Uno dei modi più comuni è utilizzare il metodo `toLowerCase()` che è disponibile sulla classe `String`. Ecco un esempio di codice che ne mostra l'utilizzo:

```Java
String stringa = "CIAO MONDO!";
String stringaMinuscola = stringa.toLowerCase();
System.out.println(stringaMinuscola);
```

Output: `"ciao mondo!"`

In questo esempio, stiamo convertendo la stringa `"CIAO MONDO!"` in lettere minuscole e stampandola a schermo. Se si esegue questo codice, il risultato sarà `"ciao mondo!"`.

Un altro modo per convertire una stringa in lettere minuscole è utilizzare il metodo `toLowerCase(Locale locale)` che consente di specificare una determinata impostazione locale per la conversione. Ad esempio, se si desidera convertire una stringa in lettere minuscole seguendo le regole della lingua italiana, si potrebbe fare così:

```Java
String stringa = "CIAO MONDO!";
String stringaMinuscola = stringa.toLowerCase(Locale.ITALIAN);
System.out.println(stringaMinuscola);
```

Output: `"ciao mondo!"`

Si può anche utilizzare il metodo `String.toLowerCase()` su un oggetto `StringBuilder` per convertire le stringhe contenute all'interno di quest'ultimo in lettere minuscole.

```Java
StringBuilder sb = new StringBuilder();
sb.append("CIAO ");
sb.append("MONDO!");
String stringa = sb.toString();
String stringaMinuscola = stringa.toLowerCase();
System.out.println(stringaMinuscola);
```

Output: `"ciao mondo!"`

## Approfondimento
La conversione di una stringa in lettere minuscole è più di una semplice operazione di formattazione del testo. In realtà, utilizza le regole della lingua specificata dalla vostra impostazione locale per stabilire quali lettere sono maiuscole e quali sono minuscole. Ad esempio, nella lingua italiana, la lettera `"i"` è sempre minuscola, mentre la lettera `"I"` è sempre maiuscola. Tuttavia, in alcune lingue, come il turco, ci sono esempi in cui la stessa lettera può essere sia maiuscola che minuscola a seconda del contesto.

C'è anche un altro aspetto importante da considerare quando si converte una stringa in lettere minuscole, e cioè l'encoding Unicode. Ci sono alcuni caratteri speciali che possono avere diverse rappresentazioni Unicode per le lettere maiuscole e minuscole. Ad esempio, la lettera `"ß"` in tedesco ha due forme maiuscole differenti ("SS" o "ẞ") e due forme minuscole differenti ("ß" o "ss"). Ciò significa che quando si converte una stringa in lettere minuscole, il risultato dipende dall'encoding Unicode specificato.

## Vedi anche
- [Documentazione ufficiale di Java per il metodo toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Articolo su Stack Overflow sulla conversione di una stringa in lettere minuscole in Java](https://stackoverflow.com/questions/1086123/how-to-get-lowercase-accented-characters-to-uppercase)
- [Guida per la formattazione del testo in Java](https://www.geeksforgeeks.org/formattin