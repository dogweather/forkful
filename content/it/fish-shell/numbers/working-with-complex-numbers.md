---
date: 2024-01-26 04:40:05.904963-07:00
description: "I numeri complessi estendono l'idea delle linee numeriche unidimensionali\
  \ a un piano complesso bidimensionale. I programmatori li utilizzano in campi come\u2026"
lastmod: 2024-02-19 22:05:02.923133
model: gpt-4-0125-preview
summary: "I numeri complessi estendono l'idea delle linee numeriche unidimensionali\
  \ a un piano complesso bidimensionale. I programmatori li utilizzano in campi come\u2026"
title: Lavorare con i numeri complessi
---

{{< edit_this_page >}}

## Cosa & Perché?
I numeri complessi estendono l'idea delle linee numeriche unidimensionali a un piano complesso bidimensionale. I programmatori li utilizzano in campi come l'ingegneria, la fisica e la grafica per calcoli che richiedono due componenti, come segnali o rotazioni.

## Come fare:
In Fish, gestiamo i numeri complessi utilizzando `math` con parti reale e immaginaria. Ecco una partenza rapida:

```fish
# Sommare due numeri complessi (3+4i) e (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Risultato: 8+6i

# Moltiplicare due numeri complessi (1+2i) e (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Risultato: -5+10i
```

Se hai bisogno di elevare un numero complesso a una potenza o ottenere la sua forma esponenziale:

```fish
# Quadrato di (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Risultato: -5+12i

# Esponenziale di (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Risultato: -0.41615+0.9093i
```

## Approfondimento
Il supporto di Fish Shell per i numeri complessi è relativamente nuovo, iniziato intorno alla versione 3.1.0. Prima di allora, le persone potrebbero aver usato `bc` o fatto ricorso a strumenti esterni come Python per la matematica complessa.

Alternative alla matematica di Fish includono librerie numeriche specializzate o linguaggi come MATLAB, Python con NumPy, o anche C++ con la Standard Library. Tuttavia, queste potrebbero essere eccessive per rapidi calcoli da shell.

Il supporto per i numeri complessi in Fish è integrato nel suo comando interno `math`, sfruttando libcalc. Questo significa che non devi installare strumenti aggiuntivi per le operazioni basilari.

Tuttavia, Fish non è progettato per il calcolo matematico pesante. La sua capacità matematica è conveniente per calcoli rapidi o script in cui entrano in gioco i numeri complessi, ma considera strumenti più robusti per compiti intensivi.

## Vedi Anche
- Documentazione di Fish shell per math: https://fishshell.com/docs/current/commands.html#math
- NumPy per Python, un'alternativa popolare: https://numpy.org/
- Uno sguardo approfondito ai numeri complessi: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
