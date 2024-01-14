---
title:                "Elixir: Utilizzare le espressioni regolari"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se vi state avventurando nel mondo della programmazione in Elixir, è probabile che vi siate imbattuti nel concetto delle espressioni regolari. Ma perché dovrebbero interessarvi? Semplice: le espressioni regolari sono uno strumento potente per cercare e manipolare testo all'interno dei vostri programmi.

## Come fare

Per utilizzare le espressioni regolari in Elixir, avrete bisogno di conoscere l'operatore `=~`. Questo operatore confronta una stringa con un'espressione regolare e restituisce `true` se trova una corrispondenza, altrimenti `false`. Ad esempio, supponiamo di avere una stringa contenente un indirizzo email e vogliamo verificarne la validità. Possiamo farlo utilizzando l'espressione regolare `\w+@\w+\.\w+`.

```
Elixir
email = "esempio@email.com"
email =~ \w+@\w+\.\w+
# output: true
```

Inoltre, potete utilizzare il modulo `Regex` per l'applicazione di operazioni più complesse sulle espressioni regolari. Ad esempio, se vogliamo estrarre il dominio dall'indirizzo email sopra, possiamo utilizzare il seguente codice:

```
Elixir
Regex.run(~r/(\w+)@(\w+)\.(\w+)/, email)
# output: ["esempio", "email", "com"]
```

## Approfondimento

Le espressioni regolari in Elixir sono potenti perché supportano diverse funzionalità avanzate, come la possibilità di utilizzare gruppi di cattura, negazione e quantificatori. Inoltre, Elixir supporta anche modelli di espressioni regolari in stile Perl, che sono più flessibili e compatibili con altri linguaggi di programmazione.

Un'altra caratteristica utile delle espressioni regolari in Elixir è la possibilità di utilizzarle nel modulo `String` per la ricerca e la sostituzione di testo all'interno di stringhe.

Per un elenco completo delle funzionalità supportate e dei vari modelli di espressioni regolari, consultate la documentazione ufficiale di Elixir.

## Vedi anche

- [Documentazione ufficiale di Elixir su espressioni regolari](https://hexdocs.pm/elixir/1.12/regex/Regex.html)
- [Regex101: Un ottimo strumento per testare le vostre espressioni regolari](https://regex101.com/)
- [Elixir Forum: Discussione sulla manipolazione di espressioni regolari in Elixir](https://elixirforum.com/t/manipulating-regex-in-elixir/9333)