---
title:                "Gestione degli errori"
date:                  2024-01-26T00:56:49.156281-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"
programming_language: "Python"
category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Gestire gli errori in Python (o in qualsiasi linguaggio di programmazione) significa aspettarsi l'inaspettato – è l'arte di gestire con eleganza quando le cose vanno storte nel tuo codice. Lo facciamo per prevenire i crash, guidare gli utenti e rendere i nostri programmi robusti e affidabili.

## Come fare:

``` Python
# Blocco try-except di base
try:
    # codice a rischio
    numero = int(input("Inserisci un numero: "))
except ValueError:
    # gestire l'errore
    print("Questo non è un numero!")

# Specificare eccezioni multiple
try:
    # codice che potrebbe sollevare diverse eccezioni
    risultato = 10 / int(input("Inserisci un divisore: "))
except ZeroDivisionError:
    print("Oops! Impossibile dividere per zero.")
except ValueError:
    print("Ho bisogno di un numero, amico.")

# Utilizzo di else e finally
try:
    numero = int(input("Inserisci un numero per quadrarlo: "))
except ValueError:
    print("Ho detto un numero!")
else:
    # nessun errore si è verificato
    print("Il tuo numero al quadrato è:", numero**2)
finally:
    # esegue sempre
    print("Grazie per averlo provato!")
```

Esempio di output quando si inserisce un numero non valido per il primo blocco:
```
Inserisci un numero: ciao
Questo non è un numero!
```

## Approfondimento

Dall'alba della programmazione, la gestione degli errori è stata cruciale. I primi approcci erano rudimentali, come verificare le condizioni prima di ogni operazione a rischio. La sintassi `try-except` di Python deriva da una tradizione di gestione delle eccezioni in linguaggi più vecchi come C++ e Java, semplificando il processo.

Quando provi un blocco di codice con `try`, Python è in cerca di eventuali eccezioni. Se compare un errore, il blocco `except` lo intercetta. Puoi essere specifico sulle eccezioni che catturi o catturarle tutte con un `except` generico. Tuttavia, essere specifici fin dall'inizio è il metodo migliore – è preciso, non una rete a strascico.

`else` e `finally` sono extra in questo concetto. Il blocco `else` viene eseguito se il blocco try è privo di errori. `finally` è l'amico affidabile che viene eseguito comunque – pensa alle operazioni di pulizia.

Alternative? Certo che ci sono. Alcuni linguaggi utilizzano codici di ritorno invece delle eccezioni. Potresti anche imbatterti in istruzioni `with` per la gestione delle risorse o `assertions` che verificano le condizioni durante lo sviluppo. Ma quando parliamo di strategie solide per la gestione degli errori, il modello try-catch si distingue per la sua leggibilità e struttura.

## Vedi anche

Ecco alcune buone risorse aggiuntive per approfondire ulteriormente:

- Documentazione ufficiale di Python sugli errori e le eccezioni: [Python Docs – Errors and Exceptions](https://docs.python.org/3/tutorial/errors.html)
- La guida di Real Python sull'argomento: [Real Python - Il blocco try/except/else/finally](https://realpython.com/python-exceptions/)
- Una discussione approfondita sulle migliori pratiche di gestione degli errori: [Stack Overflow – Come ignorare correttamente le eccezioni?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)