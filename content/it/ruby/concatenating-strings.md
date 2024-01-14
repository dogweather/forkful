---
title:                "Ruby: Unendo stringhe"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Perché

Il concatenamento di stringhe è un concetto fondamentale nella programmazione Ruby. Consiste nel combinare due o più stringhe per formarne una sola. Questa operazione è essenziale per creare output personalizzati, manipolare i dati e svolgere altre operazioni che richiedono la modifica delle stringhe.

# Come fare

Il concatenamento di stringhe è un'operazione semplice che può essere eseguita in diversi modi in Ruby. Ecco alcuni esempi:

```Ruby
# Utilizzando l'operatore +
nome = "Maria"
cognome = "Rossi"
nome_completo = nome + cognome
puts nome_completo # Output: MariaRossi

# Utilizzando il metodo concat
nome = "Maria"
cognome = "Rossi"
nome.concat(" ").concat(cognome)
puts nome # Output: Maria Rossi

# Utilizzando il metodo <<
nome = "Maria"
nome << " "
nome << "Rossi"
puts nome # Output: Maria Rossi
```

Come si può vedere dagli esempi, sia l'operatore + che i metodi concat e << possono essere utilizzati per concatenare le stringhe. È importante notare che l'operatore + restituirà una nuova stringa mentre i metodi concat e << modificheranno la stringa originale.

# Approfondimento

È possibile concatenare qualsiasi tipo di dato in Ruby, non solo le stringhe. Ad esempio, è possibile concatenare interi, float, booleani e persino oggetti.

```Ruby
# Concatenamento di interi
numero1 = 123
numero2 = 456
puts numero1 + numero2 # Output: 579

# Concatenamento di oggetti
oggetto1 = "ruby "
oggetto2 = "programming"
puts oggetto1 + oggetto2 # Output: ruby programming
```

Inoltre, Ruby offre anche il metodo .to_s che può essere utilizzato per convertire altri tipi di dato in stringhe prima di concatenarle.

```Ruby
# Utilizzando il metodo .to_s
numero = 123
puts "L'anno corrente è " + numero.to_s # Output: L'anno corrente è 123
```

# Vedi anche

- [Concatenamento di stringhe in Ruby](https://www.rubyguides.com/2019/05/concatenate-strings-in-ruby/)
- [Ruby String class documentation](https://ruby-doc.org/core/String.html)
- [Ruby concatenation operator documentation](https://ruby-doc.org/core-2.6/String.html#method-i-252B)