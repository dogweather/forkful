---
title:    "Ruby: Capitalizzare una stringa"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché
Perché dovresti mai capitalizzare una stringa? Ci sono molteplici motivi per farlo, come l'esigenza di uniformare il formato di una frase o di rendere più leggibile un testo.

## Come
Per capitalizzare una stringa in Ruby, puoi utilizzare il metodo `.capitalize`, che trasforma il primo carattere di una parola in maiuscolo e gli altri in minuscolo. Ecco un esempio di codice che mostra come utilizzarlo:

```Ruby
# Definiamo una stringa
stringa = "ciao mondo"

# Applichiamo il metodo .capitalize ad essa
puts stringa.capitalize

# Output: Ciao mondo
```

Se vuoi capitalizzare l'intera stringa invece che solo la prima parola, puoi utilizzare il metodo `.upcase`:

```Ruby
# Applichiamo il metodo .upcase alla stringa
puts stringa.upcase

# Output: CIAO MONDO
```

## Approfondimento
Ci sono anche altri metodi in Ruby che permettono di capitalizzare una stringa, come ad esempio `.capitalize!`, che modifica la stringa originale invece di crearne una nuova, o `.titleize`, che rende maiuscole le iniziali di tutte le parole in una frase.

Inoltre, è possibile specificare delle eccezioni per le parole che si desidera mantenere in minuscolo, utilizzando il metodo `.capitalize` in combinazione con il metodo `.gsub`:

```Ruby
# Definiamo una stringa
stringa = "ho voglia di un caffè"

# Applichiamo il metodo .capitalize in combinazione con .gsub
puts stringa.capitalize.gsub("un", "Un")

# Output: Ho voglia di Un caffè
```

## Vedi anche
- [Documentazione ufficiale di Ruby su String#capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- [Tutorial su come capitalizzare una stringa in Ruby](https://www.tutorialspoint.com/how-to-capitalize-a-string-in-ruby)