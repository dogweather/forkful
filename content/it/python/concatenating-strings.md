---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
La concatenazione di stringhe in Python è il processo di unione di due o più stringhe per formare una singola stringa. I programmatori la usano per manipolare e presentare i testi in modo più efficace e per facilitare le operazioni sui dati stringa.

## Come fare:
In Python possiamo concatenare le stringhe in diversi modi. Vediamo qualche esempio.

```Python
# Usando l'operatore '+':
stringa1 = "Ciao "
stringa2 = "Mondo"
stringa_unificata = stringa1 + stringa2
print(stringa_unificata)  # Output: Ciao Mondo

# Usando la funzione join():
lista_stringhe = ["Buongiorno", "a", "tutti"]
stringa_unificata = " ".join(lista_stringhe)
print(stringa_unificata)  # Output: Buongiorno a tutti

# Usando il simbolo '%':
nome = "Gio"
stringa_unificata = "Ciao %s!" % nome
print(stringa_unificata)  # Output: Ciao Gio!
```

## Approfondimento 
Concatenare stringhe è una pratica fondamentale in programmazione, risalente al primo linguaggio di programmazione. Come abbiamo visto prima, ci sono molti modi alternativi per farlo in Python. Tuttavia, è importante ricordare che alcuni metodi potrebbero essere più efficienti: usando l'operatore '+' in un ciclo, per esempio, potrebbe non essere ottimale dato che Python crea una nuova stringa ogni volta che concateniamo.

Un'altra alternativa è l'uso del metodo `format()`, il quale consente la sostituzione e la formattazione delle stringhe:
```Python
stringa1 = "Ciao"
stringa2 = "Mondo"
stringa_unificata = "{} {}".format(stringa1, stringa2)
print(stringa_unificata)  # Output: Ciao Mondo
```

O la stringa f-format nelle versioni di Python 3.6 e successive, che migliora la leggibilità del codice:
```Python
stringa1 = "Ciao"
stringa2 = "Mondo"
stringa_unificata = f"{stringa1} {stringa2}"
print(stringa_unificata)  # Output: Ciao Mondo
```

## Per saperne di più 
Per una panoramica più dettagliata sulla concatenazione di stringhe, considera i seguenti link:
- Documentazione ufficiale di Python su [Stringhe](https://docs.python.org/3/tutorial/introduction.html#strings)
- Post del blog Real Python su [String Formatting in Python](https://realpython.com/python-string-formatting/)
- Post del blog Stack Abuse su [Python String Concatenation](https://stackabuse.com/python-string-concatenation/)