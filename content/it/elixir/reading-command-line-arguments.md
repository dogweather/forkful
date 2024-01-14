---
title:    "Elixir: Lettura degli argomenti dalla linea di comando"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Molte volte nella programmazione, è necessario che il programma legga e utilizzi gli argomenti passati dalla riga di comando. Questi possono essere utili per passare informazioni al programma o per fornire opzioni diverse durante l'esecuzione. Leggere gli argomenti da riga di comando è un'operazione importante e fondamentale da conoscere per poter scrivere codice Elixir efficace.

## Come fare

Per leggere gli argomenti da riga di comando in Elixir, è necessario importare il modulo `OptionParser` e definire un'opzione da cercare. Ad esempio, se vogliamo leggere il primo argomento dalla riga di comando, possiamo definire il seguente codice all'interno di un blocco ```Elixir...```:

```Elixir
options = %{
  argument: :string
}

OptionParser.parse(args, options)
```

Dove `args` è l'array degli argomenti passati dalla riga di comando. Questo codice ci permette di leggere il valore del primo argomento utilizzando `parsed <option>`, dove `<option>` è il nome dell'opzione definita in `options`.

### Esempio:

Se il nostro programma viene chiamato da riga di comando come `elixir myprogram.exs hello`, il valore di `parsed :argument` sarà `"hello"`.

Inoltre, se vogliamo leggere più di un argomento, possiamo semplicemente definire più opzioni all'interno di `options` e utilizzarle in seguito nello stesso modo.

## Analisi approfondita

Oltre a leggere gli argomenti passati dalla riga di comando, possiamo anche fornire opzioni con valori predefiniti in caso l'utente non li specifichi durante l'esecuzione. Possiamo farlo aggiungendo l'opzione `default <value>` dopo la definizione del tipo di dato di un'opzione. Inoltre, possiamo anche utilizzare `multi true` per consentire l'utilizzo di più opzioni con lo stesso nome.

Ad esempio:

```Elixir
options = %{
  first_name: {:string, default: "John"},
  last_name: {:string, default: "Doe", multi: true}
}
```

Ciò consentirà al programma di leggere l'argomento `--first_name` con un valore predefinito di `"John"` e l'argomento `--last_name` con un valore predefinito di `"Doe"`, ma anche, se specificato, di utilizzare più volte l'opzione `--last_name` per leggere più valori.

## Vedi anche

- Documentazione ufficiale di Elixir sulla gestione degli argomenti da riga di comando: https://hexdocs.pm/elixir/OptionParser.html
- Un tutorial su come utilizzare gli argomenti da riga di comando in Elixir: https://medium.com/@sfogo/using-command-line-options-in-elixir-f318f188df
- Una spiegazione più approfondita sul funzionamento di `OptionParser`: https://medium.com/@mattiaocchiuto/elixir-and-command-line-arguments-using-optionparser-ef22fda9fca6