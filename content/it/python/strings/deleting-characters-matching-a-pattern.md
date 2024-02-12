---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
aliases:
- /it/python/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:43:11.016280-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Che cosa e perché?)
Nella programmazione, eliminare caratteri che corrispondono a un pattern significa rimuovere specifiche sequenze di caratteri da una stringa. Lo facciamo per pulire i dati, validare l'input, o per preparare le stringhe per ulteriori elaborazioni.

## How to (Come fare)
```Python
import re

# Esempio base: rimuovere tutti i punti dalla stringa
stringa = "Esempio.di.stringa.con.punti."
pattern = "\."
stringa_senza_punti = re.sub(pattern, "", stringa)
print(stringa_senza_punti)

# Esempio con numeri: rimuovere tutte le cifre
stringa_numeri = "Anno2023, Mese 03."
pattern_numeri = "\d"
stringa_senza_numeri = re.sub(pattern_numeri, "", stringa_numeri)
print(stringa_senza_numeri)

# Esempio con caratteri speciali: rimuovere i segni di punteggiatura
import string

stringa_punteggiatura = "Ciao! Come va? Tutto bene."
stringa_pulita = stringa_punteggiatura.translate(str.maketrans("", "", string.punctuation))
print(stringa_pulita)
```
Output:
```
Esempiodistringaconpunti
Anno, Mese .
Ciao Come va Tutto bene
```

## Deep Dive (Approfondimento)
Cancellare caratteri seguendo un pattern in Python è spesso eseguito con il modulo `re`, che sta per regular expression (espressioni regolari). Dall'introduzione delle espressioni regolari negli anni '50, sono diventate uno strumento essenziale per la manipolazione di stringhe.

Le alternative all'uso delle regex possono essere metodi più semplici come `str.replace()` per sostituire occorrenze specifiche o `str.translate()` per una rimozione più generale dei caratteri. Per espressioni regolari complesse, le performance possono essere migliorate con l'utilizzo di moduli esterni come `regex`.

Il modulo `regex` permette maggiore flessibilità e funzionalità rispetto al modulo `re` integrato. Tuttavia, `re` resta il punto di partenza per la maggior parte dei programmatori data la sua inclusione standard in Python.

## See Also (Vedi anche)
- Documentazione Python sul modulo `re`: https://docs.python.org/3/library/re.html
- Esercizi sulle espressioni regolari: https://regexone.com/
- Tutorial ufficiale Python sulle stringhe e le espressioni regolari: https://docs.python.org/3/howto/regex.html
- Documentazione sul modulo `regex`, alternativa a `re`: https://pypi.org/project/regex/
