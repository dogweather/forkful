---
title:    "Elixir: Cancellazione dei caratteri che corrispondono a un modello"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Ci sono molteplici motivi per cui si potrebbe voler eliminare caratteri che corrispondono ad un certo pattern in Elixir. Ad esempio, si potrebbe voler pulire una stringa prima di elaborarla ulteriormente o rimuovere caratteri speciali che potrebbero causare problemi in un'applicazione.

## Come Fare

Per eliminare i caratteri che corrispondono ad un certo pattern, possiamo utilizzare il metodo `String.replace/4`. Prenderemo come esempio una stringa contenente numeri di telefono con diversi formati, come ad esempio "(555) 123-4567", "555-123-4567" o "5551234567".

Iniziamo definendo una funzione che prende in input una stringa e un pattern da cercare all'interno di essa:

```elixir
def clean_string(input_string, pattern) do
  String.replace(input_string, pattern, "")
end
```

Dopo di ciò, possiamo chiamare la funzione utilizzando diversi pattern, come ad esempio `"[^0-9]"` per eliminare tutti i caratteri diversi dai numeri:

```elixir
clean_string("(555) 123-4567", "[^0-9]") #=> "5551234567"
clean_string("555-123-4567", "[^0-9]") #=> "5551234567"
clean_string("5551234567", "[^0-9]") #=> "5551234567"
```

Possiamo anche utilizzare espressioni regolari per eliminare solo alcuni caratteri specifici. Ad esempio, se volessimo eliminare solo le parentesi e il trattino dalla stringa, useremmo il pattern `"[()-]"`:

```elixir
clean_string("(555) 123-4567", "[()-]") #=> "555 1234567"
clean_string("555-123-4567", "[()-]") #=> "555 1234567"
```

Come si può notare, il metodo `String.replace/4` ci permette di rimuovere facilmente i caratteri che non ci interessano dalla nostra stringa, lasciandoci solo con i numeri.

## Approfondimento

Nell'esempio sopra, abbiamo utilizzato il metodo `String.replace/4` passandogli due parametri e ottenendo una nuova stringa come risultato. Tuttavia, questo metodo ci permette anche di eseguire operazioni più complesse grazie al quarto parametro, ovvero una funzione che viene eseguita su ogni corrispondenza del pattern.

Per esempio, se volessimo trasformare tutti i numeri della stringa in lettere, potremmo utilizzare la seguente funzione e passarla come quarto parametro al metodo `String.replace`:

```elixir
def numbers_to_letters(match) do
  String.to_charlist(String.to_integer(match) + 64)
end
```

La funzione prende in input la corrispondenza del pattern (in questo caso, un numero) e lo trasforma in una lettera corrispondente (ad esempio, '3' in 'C'). Chiamando la funzione `clean_string` precedentemente definita con questa funzione come quarto parametro, otterremo una nuova stringa con tutte le lettere al posto dei numeri.

## Vedi Anche

- [Documentazione su String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [Espressioni regolari in Elixir](https://elixir-lang.org/getting-started/regex.html)
- [Metodi della libreria standard di Elixir](https://hexdocs.pm/elixir/Kernel.html)