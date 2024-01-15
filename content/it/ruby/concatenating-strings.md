---
title:                "Concatenazione di stringhe"
html_title:           "Ruby: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un aspetto importante della programmazione perché permette di unire insieme diverse parole o frasi per creare una stringa più lunga. Questa funzione è utile in molte situazioni, ad esempio nella creazione di messaggi personalizzati o nella costruzione di URL.

## Come Fare

Per concatenare stringhe in Ruby, possiamo utilizzare l'operatore di concatenazione `+` oppure il metodo `concat`. Ecco un esempio di entrambi i metodi:

```Ruby
stringa1 = "Ciao"
stringa2 = "mondo"

# Utilizzando l'operatore di concatenazione
concatenata = stringa1 + " " + stringa2
puts concatenata
# Output: Ciao mondo

# Utilizzando il metodo concat
concatenata = stringa1.concat(" ", stringa2)
puts concatenata
# Output: Ciao mondo
```

In entrambi i casi, abbiamo ottenuto lo stesso risultato: la stringa concatenata "Ciao mondo". Nota che è importante includere gli spazi vuoti tra le parole per ottenere una stringa ben formattata.

Inoltre, possiamo concatenare più di due stringhe, semplicemente aggiungendole in sequenza:

```Ruby
saluto = "Buongiorno"
nome = "Mario"
cognome = "Rossi"

saluto_completo = saluto + " " + nome + " " + cognome

puts saluto_completo
# Output: Buongiorno Mario Rossi
```

## Approfondimento

Per fare un po' di chiarezza, andiamo a scoprire alcune cose interessanti su come funziona esattamente la concatenazione di stringhe in Ruby.

Innanzitutto, vediamo che il metodo `concat` in realtà è un'istanza della classe `String`, che eredita il metodo dalla classe `Object`. Ciò significa che possiamo anche chiamarlo utilizzando la sintassi del punto:

```Ruby
stringa1 = "Buongiorno"
stringa2 = "mondo"

saluto = stringa1.concat(" ", stringa2)

puts saluto
# Output: Buongiorno mondo
```

Inoltre, vale la pena notare che quando utilizziamo l'operatore di concatenazione `+`, in realtà stiamo chiamando il metodo `+` della classe `String`. Questo metodo accetta un argomento e restituisce una nuova stringa contenente l'unione delle due stringhe:

```Ruby
stringa1 = "Ciao"
stringa2 = "mondo"

concatenata = stringa1 + " " + stringa2
# Equivalente a: stringa1.+(stringa2)

puts concatenata
# Output: Ciao mondo
```

Infine, possiamo anche utilizzare la funzione `format` della classe `String` per concatenare stringhe in modo più efficiente. Questa funzione accetta un numero indefinito di argomenti e ha la possibilità di formattare la stringa in base ad essi. Ecco un esempio:

```Ruby
numero = 5
descrizione = "palle"

stringa = "Ho comprato %d %s di natale." % [numero, descrizione]
# Output: Ho comprato 5 palle di natale.
```

## Vedi Anche

- [String documentation in Ruby](https://ruby-doc.org/core-2.7.1/String.html)
- [The Basics of Ruby String Concatenation](https://www.rubyguides.com/2019/10/ruby-string-concatenation/)
- [Ruby's String Concatenation Operator Explained](https://www.honeybadger.io/blog/ruby-string-concatenation-operator/)