---
title:                "Interpolazione di una stringa"
date:                  2024-01-20T17:51:24.808136-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolazione di una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
L'interpolazione di stringhe inserisce valori di variabili all'interno di una stringa. È utile per personalizzare messaggi, costruire output facilmente leggibili e scrivere codice più pulito e manutenibile.

## How to: (Come farlo:)
Python permette l'interpolazione di stringhe in diversi modi. Ecco alcuni esempi con l'output corrispondente.

```Python
# Utilizzando f-string (disponibile da Python 3.6 in poi)
nome = "Giovanni"
messaggio = f"Ciao {nome}, come stai?"
print(messaggio)  # Output: Ciao Giovanni, come stai?

# Metodo format()
nome = "Giovanni"
messaggio = "Ciao {}, come stai?".format(nome)
print(messaggio)  # Output: Ciao Giovanni, come stai?

# %-formattazione (stile più vecchio)
nome = "Giovanni"
messaggio = "Ciao %s, come stai?" % nome
print(messaggio)  # Output: Ciao Giovanni, come stai?
```

## Deep Dive (Approfondimento)
L'interpolazione di stringhe è presente in Python piuttosto da tempo. Il vecchio stile usava il carattere `%`, simile alla printf in C. Più tardi è arrivato il metodo `format()`, più potente e flessibile.

Ma la vera svolta è stata l'introduzione delle f-string in Python 3.6. Usando un prefisso `f`, puoi direttamente incorporare espressioni Python all'interno delle stringhe. Il codice risulta più leggibile, e l'esecuzione è anche più veloce.

Come alternativa a queste opzioni built-in, esistono librerie di terze parti (come `jinja2` o `mako`) che offrono funzionalità di templating avanzate, utili in applicazioni web e non solo.

Dal punto di vista dell'implementazione, quando Python processa una f-string, la converte in una serie di operazioni di concatenazione e formattazione, ottimizzando dove possibile.

## See Also (Vedi Anche)
- Documentazione ufficiale su f-string: https://docs.python.org/3/reference/lexical_analysis.html#f-strings
- Documentazione ufficiale su `str.format()`: https://docs.python.org/3/library/stdtypes.html#str.format
- Informazioni sulla %-formattazione: https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting
- `jinja2`: https://palletsprojects.com/p/jinja/
- `mako`: https://www.makotemplates.org/
