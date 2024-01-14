---
title:                "Fish Shell: Concatenazione di stringhe"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Spoiler alert: concatenare le stringhe è una tecnica molto utile in Fish Shell! Ti permette di combinare più stringhe in una sola, rendendo il tuo codice più efficiente e conciso. Continua a leggere per scoprire come farlo.

## Come Fare

In Fish Shell, puoi concatenare le stringhe utilizzando l'operatore `+`. Ad esempio, se voglio combinare le parole "Ciao" e "Mondo" in una sola stringa, posso scrivere:

```
Fish Shell> echo "Ciao" + "Mondo"
CiaoMondo
```

Se vuoi aggiungere uno spazio tra le due parole, puoi farlo includendo il carattere vuoto tra le due stringhe:

```
Fish Shell> echo "Ciao" + " " + "Mondo"
Ciao Mondo
```

Questo è solo un esempio di come puoi utilizzare l'operatore `+` per combinare le stringhe. Puoi anche utilizzare variabili, comandi e altro ancora per aggiungere ulteriori elementi alla tua stringa concatenata.

## Approfondimento

Concatenare le stringhe può essere utile anche quando si utilizzano cicli. Ad esempio, se vuoi creare una stringa che contenga i nomi dei tuoi amici separati da virgole, puoi fare qualcosa del genere:

```
Fish Shell> set amici "Marco" "Anna" "Luca"
Fish Shell> set lista ""
Fish Shell> for amico in $amici
  lista = $lista + $amico + ", "
end
Fish Shell> echo $lista
Marco, Anna, Luca, 
```

Come puoi vedere, utilizzando l'operatore `+` all'interno del ciclo, possiamo aggiungere ogni nome alla variabile `lista` e ottenere una stringa concatenata con tutti i nomi dei nostri amici.

## Vedi Anche

- Documentazione ufficiale sulla concatenazione delle stringhe in [Fish Shell](https://fishshell.com/docs/current/index.html#string);
- Un esempio di come utilizzare le stringhe concatenate in [uno script bash](https://www.howtogeek.com/281010/write-a-bash-script-to-turn-text-to-speech/);
- Una spiegazione dettagliata su come funziona la concatenazione delle stringhe in [JavaScript](https://www.freecodecamp.org/news/the-basics-of-string-concatenation-in-javascript-a1b05b2c18b1/).