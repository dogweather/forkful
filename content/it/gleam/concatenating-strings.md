---
title:    "Gleam: Unendo stringhe"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione comune nella programmazione e consiste nel combinare più stringhe in una sola. Ciò può essere utile per creare messaggi dinamici o per formattare i dati in modo specifico. In Gleam, la concatenazione di stringhe può essere fatta in modo semplice e veloce utilizzando l'operatore "+". 

## Come fare 

Per concatenare due stringhe, basta utilizzare l'operatore "+". Ad esempio: 

```Gleam
let saluto = "Ciao" + "Mondo"
```

L'output di questo codice sarà "CiaoMondo". È importante notare che, poiché Gleam è un linguaggio staticamente tipizzato, è necessario che entrambe le stringhe abbiano lo stesso tipo. In caso contrario, si otterrà un errore durante la compilazione. 

Un'altra opzione è l'utilizzo della funzione `String.concat()` che consente di unire più stringhe in una sola. Ad esempio: 

```Gleam
let nome = "Maria"
let cognome = "Rossi"
let nome_completo = String.concat([nome, cognome])
```

L'output di questo codice sarà "MariaRossi". Nota che `String.concat()` accetta una lista di stringhe come argomento. Questo rende più semplice la concatenazione di un numero variabile di stringhe.

## Approfondimento

La concatenazione di stringhe può sembrare semplice, ma è importante tenere in considerazione alcuni aspetti. Ad esempio, è bene evitare di concatenare un grande numero di stringhe in una sola operazione, poiché questo potrebbe causare un rallentamento delle prestazioni. In questo caso, potrebbe essere più efficiente utilizzare il tipo di dato `io_string_builder` che consente di concatenare le stringhe in maniera più efficiente. 

Inoltre, è importante prestare attenzione alla gestione degli spazi bianchi. Se si desidera concatenare stringhe con uno spazio nel mezzo, è necessario inserirlo manualmente, altrimenti le stringhe verranno combinate senza spazi. 

## Vedi anche 

- [Documentazione del tipo di dato `io_string_builder`](https://gleam.run/types/io_string_builder/)
- [Esempio di utilizzo di `io_string_builder` per concatenare stringhe](https://gleam.run/snippets/io_string_builder/)