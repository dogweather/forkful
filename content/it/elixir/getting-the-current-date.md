---
title:    "Elixir: Ottenerà la data attuale"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché 
Se sei un programmatore Elixir, probabilmente ti sei chiesto come ottenere la data corrente in modo efficiente. In questo articolo, scoprirai come farlo utilizzando Elixir!

## Come Fare 
Per ottenere la data corrente, puoi utilizzare la funzione `Date.utc_today/0` di Elixir. Ecco un esempio di codice che mostra come utilizzarla e l'output che restituisce:

```Elixir 
today = Date.utc_today() 
IO.puts(today)
```

Output: `#Data<2022-01-21>`

In questo esempio, abbiamo utilizzato la funzione `IO.puts/1` per stampare la data corrente sullo schermo. Tieni presente che il formato della data potrebbe variare a seconda delle impostazioni locali del tuo sistema, ma la data restituita sarà sempre in formato UTC.

Se hai bisogno di ottenere anche l'ora corrente, puoi utilizzare la funzione `Time.utc_now/0`, che ti restituirà un'ora nella zona oraria UTC. Ecco un esempio di codice:

```Elixir 
now = Time.utc_now() 
IO.puts(now)
```

Output: `#Time<2022-01-21 12:00:00Z>`

## Approfondimento 
Se vuoi esplorare ulteriormente il concetto di date e orari in Elixir, puoi consultare la documentazione ufficiale di Elixir sulle date e sugli orari. Inoltre, se vuoi manipolare le date in modo più specifico, puoi utilizzare il modulo `Calendar`, che offre numerose funzioni per manipolare date, orari e tempi.

## Vedi Anche 
- [Documentazione ufficiale di Elixir sulle date e gli orari](https://hexdocs.pm/elixir/Calendar.html#content)
- [Elixir School: Date e Orari](https://elixirschool.com/it/lessons/advanced/dates-times/)