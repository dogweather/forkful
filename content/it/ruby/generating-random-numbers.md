---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generazione di Numeri Casuali in Ruby

## Che Cosa e Perché?

La generazione di numeri casuali è il processo di creazione di numeri in maniera imprevista senza nessun segno di regola o pattern. Questo è fondamentale per meccanismi come la crittografia, la modellazione e la simulazione, i test di unità o giochi e probabilisticamente per testare le funzionalità del nostro codice.

## Come Fare:

Ecco come generare numeri casuali in Ruby. Usiamo il metodo `rand` della classe `Kernel`.

```Ruby
# Genera un numero casuale tra 0 e 1
puts rand 
# Genera un numero casuale tra 0 e 10
puts rand(10)
```

Input del programma sopra potrebbe essere

```Ruby
0.5156214589232793
7
```

## Approfondimento

1. **Contesto Storico:** Le librerie per la generazione di numeri casuali sono presenti nel core di Ruby dal suo sviluppo iniziale negli anni '90. Il metodo `rand` esiste dalla versione 1.8.7.

2. **Alternative:** Ruby offre anche la classe `Random` per generare numeri casuali. Questo dà un controllo più fine sulla generazione.

    ```Ruby
    # Nuovo oggetto con seme casuale
    random = Random.new
    puts random.rand
    puts random.rand(100)
    ```

3. **Dettagli di Implementazione:** Il metodo `rand` implementa l'algoritmo Mersenne Twister, noto per la sua rapidità e precisione nella generazione di numeri pseudo-casuali.

## Vedi Anche

1. Documentazione ufficiale di Ruby sul metodo `rand`: http://ruby-doc.org/core-2.4.1/Kernel.html#method-i-rand
2. Classe `Random` nella documentazione ufficiale di Ruby: http://ruby-doc.org/core-2.4.1/Random.html
3. Wikipedia sull'Algoritmo Mersenne Twister: https://it.wikipedia.org/wiki/Mersenne_twister