---
title:    "Python: Estrazione di sottostringhe"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è una delle funzionalità più utili di Python. Ti permette di ottenere parti specifiche di una stringa, come ad esempio le prime tre lettere o gli ultimi cinque caratteri. Questa abilità può essere utile in varie situazioni, come ad esempio la manipolazione di dati o la creazione di output personalizzato.

## Come fare

Per estrarre una sottostringa, utilizza il seguente codice:

```Python
stringa = "Ciao a tutti!"
sottostringa = stringa[5:9]
print(sottostringa)
```

Questo codice stamperà "a tu" come output. Nella prima riga, abbiamo definito una stringa di testo. Quindi, nella seconda riga, abbiamo utilizzato la notazione degli indici per estrarre i caratteri dalla posizione 5 alla 9 (l'ultimo carattere non è incluso) e assegnarli alla variabile "sottostringa". Infine, nella terza riga, abbiamo stampato il valore della sottostringa.

Puoi anche utilizzare questa notazione per estrarre i caratteri da una posizione specifica fino alla fine della stringa, come nell'esempio seguente:

```Python
stringa = "Buonanotte"
sottostringa = stringa[4:]
print(sottostringa)
```

Questo codice stamperà "anotte" come output.

## Approfondimento

La notazione degli indici per estrarre le sottostringhe è abbastanza versatile e ti consente di manipolare in modo creativo le tue stringhe. Inoltre, è possibile utilizzare valori negativi per estrarre i caratteri dalla fine della stringa, come mostrato nell'esempio seguente:

```Python
stringa = "Ciao a tutti!"
sottostringa = stringa[-6:-1]
print(sottostringa)
```

Questo codice stamperà "e tutti" come output. Sì, è possibile estrarre più di un carattere alla volta, semplicemente aumentando l'intervallo di indici.

## Vedi anche

Ecco alcuni link utili per approfondire l'estrazione di sottostringhe in Python:

- [La documentazione ufficiale di Python sugli indici per le stringhe](https://docs.python.org/3/library/stdtypes.html#sequencetypes)
- [Un tutorial su come estrarre le sottostringhe in Python](https://www.programiz.com/python-programming/methods/string/slice)
- [Un esempio di utilizzo delle sottostringhe in un problema di programmazione su HackerRank](https://www.hackerrank.com/challenges/python-string-slicing/problem)