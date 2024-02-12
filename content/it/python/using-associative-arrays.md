---
title:                "Utilizzo di array associativi"
aliases:
- it/python/using-associative-arrays.md
date:                  2024-01-30T19:12:40.213462-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di array associativi"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Gli array associativi, noti in Python come dizionari, mappano chiavi a valori, rendendo facile recuperare, modificare o tenere traccia dei dati mediante un identificatore unico. I programmatori li utilizzano per la loro efficienza nell'accesso agli elementi e per la loro flessibilità nel rappresentare strutture dati complesse.

## Come fare:

Creare un dizionario in Python è semplice. Si racchiudono le coppie chiave-valore tra parentesi graffe `{}`, con chiavi e valori separati da due punti:

```Python
# Crea un array associativo (dizionario)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Output:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Accedere a un valore tramite la sua chiave è semplice:

```Python
# Accedi a un valore
print(my_dict["name"])
```

Output:
```
John
```

Aggiungere o aggiornare elementi si fa assegnando un valore a una chiave:

```Python
# Aggiungi una nuova coppia chiave-valore
my_dict["email"] = "john@example.com"
# Aggiorna un valore
my_dict["age"] = 31
print(my_dict)
```

Output:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

Per iterare sugli elementi del dizionario:

```Python
# Itera attraverso le coppie chiave-valore
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Output:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Approfondimento

Gli array associativi in Python, o dizionari, sono stati introdotti per fornire una struttura dati per un accesso e una manipolazione dei dati efficienti. A differenza delle sequenze, che sono indicizzate da una gamma di numeri, i dizionari sono indicizzati da chiavi, che possono essere di qualsiasi tipo immutabile. Questa scelta progettuale rende i dizionari particolarmente adatti per tabelle di ricerca rapide dove le chiavi mappano a valori unici.

Storicamente, i dizionari Python sono stati implementati utilizzando una tabella hash, garantendo che la complessità temporale media per le operazioni di ricerca, inserimento e cancellazione sia O(1). In Python 3.6 e successivi, i dizionari mantengono anche l'ordine di inserimento degli elementi, combinando i vantaggi delle tabelle hash con la prevedibilità dell'ordine di inserimento vista nelle strutture dati ordinate.

Sebbene i dizionari siano incredibilmente versatili, in alcuni casi specializzati, alternative come `collections.defaultdict` o `collections.OrderedDict` (prima di Python 3.7) potrebbero essere preferibili. `defaultdict` è particolarmente utile quando si ha bisogno di un dizionario che restituisca un valore predefinito per le chiavi inesistenti, semplificando certi tipi di logica condizionale. Tuttavia, con il continuo miglioramento ed evoluzione di Python, la classe di dizionario integrata rimane spesso la scelta preferita per gli array associativi grazie alla sua robustezza e alla comodità che offre sin dall'inizio.
