---
title:    "Elixir: Capitalizzare una stringa"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa in programmazione è spesso necessario quando si vuole rendere più leggibile il testo o uniformare formattazioni in un sistema.

## Come fare

Per capitalizzare una stringa in Elixir, è possibile utilizzare il metodo `String.capitalize/1`. Questo metodo prenderà una stringa come input e restituirà la stessa stringa con il primo carattere maiuscolo. Esempio:

```Elixir
name = "mario"
String.capitalize(name)
# Output: "Mario"
```

Se si desidera capitalizzare tutti i caratteri all'interno di una stringa, è possibile utilizzare il metodo `String.upcase/1`. Questo metodo renderà tutti i caratteri della stringa in maiuscolo. Esempio:

```Elixir
message = "benvenuti al mondo della programmazione"
String.upcase(message)
# Output: "BENVENUTI AL MONDO DELLA PROGRAMMAZIONE"
```

## Approfondimento

La differenza tra i due metodi di cui sopra è che `String.capitalize/1` si limita a cambiare il primo carattere, mentre `String.upcase/1` cambierà tutti i caratteri nella stringa, anche quelli che già sono in maiuscolo.

Inoltre, è possibile combinare più metodi per ottenere risultati specifici. Ad esempio, se si vuole solo capitalizzare il primo carattere di ogni parola in una stringa, è possibile utilizzare i metodi `String.capitalize/1` e `Enum.join/2`. Esempio:

```Elixir
title = "i segreti della programmazione"
title
|> String.split()
|> Enum.map(&String.capitalize/1)
|> Enum.join(" ")
# Output: "I Segreti Della Programmazione"
```

## Vedi anche

- [documentazione ufficiale di Elixir sulle stringhe](https://hexdocs.pm/elixir/String.html)
- [tutorial su come manipolare stringhe in Elixir](https://elixirschool.com/lessons/basics/string-manipulation/)