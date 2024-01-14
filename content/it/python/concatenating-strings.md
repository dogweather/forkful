---
title:                "Python: Unire le stringhe"
simple_title:         "Unire le stringhe"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione importante e comune in programmazione che consente di unire più stringhe in una sola. È particolarmente utile quando si vuole costruire una singola stringa da inserire in un output o quando si desidera manipolare dinamicamente il contenuto di una stringa.

## Come fare

Vi sono diverse modalità per concatenare stringhe in Python. Una delle più comuni è l'utilizzo dell'operatore "+" che permette di concatenare due o più stringhe in una sola.

```Python
stringa1 = "Ciao "
stringa2 = "Mondo!"

# Utilizzo dell'operatore "+"
nuova_stringa = stringa1 + stringa2
print(nuova_stringa)
```

Output: Ciao Mondo!

In alternativa, è possibile utilizzare la funzione "join()" per concatenare una lista di stringhe, specificando un carattere di separazione.

```Python
lista_stringhe = ["Python", "è", "un", "linguaggio", "di", "programmazione"]

# Utilizzo della funzione "join()"
nuova_stringa = " ".join(lista_stringhe)
print(nuova_stringa)
```

Output: Python è un linguaggio di programmazione

## Approfondimento

È importante tenere a mente che la concatenazione di stringhe crea un nuovo oggetto stringa anziché modificare quelli esistenti. Questo significa che se si desidera concatenare diverse stringhe all'interno di un loop, è consigliabile utilizzare una lista o un'altro tipo di oggetto mutabile in cui inserire le stringhe e poi concatenarle una volta che il loop è completato.

Inoltre, è possibile utilizzare il metodo "format()" per non solo concatenare stringhe, ma anche per formattarle in modo più elegante e dinamico.

```Python
nome = "Giovanni"
eta = 27

# Utilizzo del metodo "format()"
print("Ciao, mi chiamo {0} e ho {1} anni!".format(nome, eta))
```

Output: Ciao, mi chiamo Giovanni e ho 27 anni!

## Vedi anche

Per saperne di più sulla manipolazione delle stringhe in Python, consulta i seguenti articoli:

- [Manipolare stringhe in Python](https://www.programmareinpython.it/manipolare-stringhe-python/)
- [String concatenation in Python](https://www.geeksforgeeks.org/string-concatenation-in-python/)
- [Using the format() method for string concatenation in Python](https://www.geeksforgeeks.org/using-the-format-method-in-python/)