---
date: 2024-01-20 17:34:54.029043-07:00
description: 'How to: (Come fare:) Per concatenare stringhe in Fish, puoi semplicemente
  scrivere una dopo l''altra.'
lastmod: '2024-04-05T21:53:44.593775-06:00'
model: gpt-4-1106-preview
summary: (Come fare:) Per concatenare stringhe in Fish, puoi semplicemente scrivere
  una dopo l'altra.
title: Concatenazione di stringhe
weight: 3
---

## How to: (Come fare:)
Per concatenare stringhe in Fish, puoi semplicemente scrivere una dopo l'altra.

```Fish Shell
# Concatenazione diretta
set saluto "Ciao"
set nome "Mondo"
echo $saluto$nome
# Output: CiaoMondo
```

Vuoi uno spazio? Aggiungilo.

```Fish Shell
# Concatenazione con spazio
echo $saluto" "$nome
# Output: Ciao Mondo
```

Concatenazione di variabili con stile.

```Fish Shell
# Concatenazione usando le parentesi graffe
echo {$saluto}{$nome}
# Output: CiaoMondo

# Concatenazione con spazio usando le parentesi graffe
echo {$saluto}" "{$nome}
# Output: Ciao Mondo
```

## Deep Dive (Approfondimento)
Il concetto di concatenazione di stringhe non è niente di nuovo; è fondamentale nella programmazione sin dai suoi albori. In altri linguaggi, potresti trovare operatori appositi come `+` in Python o JavaScript. Però, con Fish, è tutto più lineare: metti le stringhe una accanto all'altra.

Alternativamente, puoi usare il comando `string join`:

```Fish Shell
string join " " $saluto $nome
# Output: Ciao Mondo
```

Sotto il cofano, Fish gestisce la concatenazione di stringhe senza richiedere caratteri speciali. Questo lo rende più pulito e meno soggetto a errori di sintassi, soprattutto per gli script più complessi o durante l'uso di variabili multiple.

## See Also (Vedi Anche)
- Documentazione ufficiale di Fish: [Concatenating Strings](https://fishshell.com/docs/current/index.html#syntax)
- Discussioni sulla concatenazione di stringhe: [GitHub Issue Tracker for fish-shell](https://github.com/fish-shell/fish-shell)
- Guide alternative sulla programmazione in Fish: [Awesome Fish](https://github.com/jorgebucaran/awsm.fish)
