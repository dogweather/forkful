---
title:    "Ruby: Scrivere su standard error"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere utile quando si vuole visualizzare un messaggio di errore o avvisi durante l'esecuzione di un programma in Ruby. Invece di stampare il messaggio su standard output, che è la solita destinazione dei messaggi di output, si può scegliere di scrivere su standard error per indicare che il messaggio è un avviso o un errore.

## Come Fare

Per scrivere su standard error in Ruby, si può utilizzare il metodo `warn` che è originariamente definito nella classe `Kernel`. Ecco un esempio di codice:

```Ruby
def divide(a, b)
  if b == 0
    warn "Errore: divisione per zero"
  else
    a / b
  end
end

divide(6, 3)
# 2
divide(10, 0)
# Errore: divisione per zero
```

Come si può vedere dall'esempio, il messaggio di errore è stato stampato su standard error invece di standard output.

## Approfondimento

In Ruby, ci sono 3 stream di output standard: standard input, standard output e standard error. L'utilizzo di standard error per gli avvisi e gli errori è consigliato poiché viene gestito in modo diverso dallo standard output. Inoltre, è possibile reindirizzare gli output su standard error usando il simbolo `2>` in linea di comando. Ad esempio, se si vuole salvare gli errori di un programma in un file chiamato `errors.txt`, si può eseguire il seguente comando:

```bash
ruby programma.rb 2> errors.txt
```

## Vedi Anche

- [Documentazione di Ruby sul metodo `warn`](https://ruby-doc.org/core-2.6.6/Kernel.html#method-i-warn)
- [Informazioni sui 3 stream di output standard](https://tldp.org/LDP/abs/html/io-redirection.html)
- [Tutorial su come gestire gli errori in Ruby](https://www.codecademy.com/learn/learn-ruby/modules/exceptions-ruby)