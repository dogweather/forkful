---
title:    "Elixir: Conversione di una data in una stringa"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Convertire una data in una stringa è una necessità comune nella programmazione Elixir. È utile per presentare le date in un formato significativo per l'utente finale o per l'interazione con database o API.

## Come fare
Per convertire una data in una stringa in Elixir, puoi utilizzare la funzione `Calendar.strftime/3`. Questa funzione accetta tre argomenti: la data da convertire, un formato di stringa e un elenco di opzioni.

```
Elixir def date_to_string(date) do
  Calendar.strftime(date, "%d/%m/%Y", [])
end

print date_to_string({{2021, 1, 15}, {}})

# Output: "15/01/2021"
```

## Approfondimento
La funzione `Calendar.strftime/3` utilizza i caratteri di formattazione di strftime di C, che possono essere consultati nella documentazione ufficiale di Elixir. Ad esempio, `%d` rappresenta il giorno del mese con due cifre e `%Y` rappresenta l'anno con quattro cifre.

Inoltre, è possibile specificare opzioni aggiuntive come il locale e il fuso orario per una maggiore personalizzazione della stringa data. Ad esempio, `[:locale, "it", :time_zone, "Europe/Rome"]` permetterà di visualizzare la data secondo le convenzioni italiane e il fuso orario di Roma.

## Vedi anche
- Documentazione ufficiale di Elixir per `Calendar.strftime/3`: https://hexdocs.pm/elixir/Calendar.html#strftime/3
- Caratteri di formattazione di strftime di C: http://man7.org/linux/man-pages/man3/strftime.3.html