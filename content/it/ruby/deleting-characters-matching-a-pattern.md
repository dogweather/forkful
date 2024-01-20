---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---

# Eliminare Caratteri Corrispondenti a un Pattern in Ruby

---

## Cos'è & Perché?
Eliminare caratteri che corrispondono a un pattern significa rimuovere tutti i caratteri che corrispondono a un determinato modello dal tuo codice. I programmatori fanno questo per pulire i dati, ottenere informazioni pertinenti e migliorare l'efficienza del codice.

## Come Fare:
Vediamo come Ruby può eliminare i caratteri che corrispondono a un determinato pattern. Useremo il metodo `delete`.
```ruby
# Definiamo una stringa
str = "Ciao, Mondo!"
# Eliminiamo tutti i caratteri che non sono lettere
str_ripulita = str.delete '^A-Za-z'

puts str_ripulita
# Output: CiaoMondo
```
In questo esempio, `^A-Za-z` è il pattern e corrisponde a tutti i caratteri che non sono lettere alfabetiche.

## Approfondimento
Il metodo `delete` è parte del nucleo di Ruby fin dalla sua creazione, ed è uno strumento indispensabile per la manipolazione delle stringhe.

Esistono alternative al metodo `delete`, come `gsub`, che può sostituire i caratteri corrispondenti a un pattern con un altro carattere o stringa.

```ruby
str = "Ciao, Mondo!"
# Usiamo gsub per sostituire tutto ciò che non è una lettera con niente
str_ripulita = str.gsub(/[^A-Za-z]/, '')

puts str_ripulita
# Output: CiaoMondo
```

Il dettaglio implementativo principale da ricordare è che `delete` è un metodo distruttivo quando usato con `!`, come `delete!`, altererà la stringa originale invece di crearne una nuova.

## Vedi Anche
Per approfondire su come eliminare i caratteri che corrispondono a un pattern in Ruby, dai un'occhiata a queste risorse: