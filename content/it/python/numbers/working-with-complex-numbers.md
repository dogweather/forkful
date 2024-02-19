---
aliases:
- /it/python/working-with-complex-numbers/
date: 2024-01-26 04:45:35.004133-07:00
description: "I numeri complessi sono un insieme di numeri della forma `a + bi`, dove\
  \ `a` e `b` sono numeri reali, e `i` \xE8 l'unit\xE0 immaginaria (`i^2 = -1`). In\u2026"
lastmod: 2024-02-18 23:08:55.515438
model: gpt-4-0125-preview
summary: "I numeri complessi sono un insieme di numeri della forma `a + bi`, dove\
  \ `a` e `b` sono numeri reali, e `i` \xE8 l'unit\xE0 immaginaria (`i^2 = -1`). In\u2026"
title: Lavorare con i numeri complessi
---

{{< edit_this_page >}}

## Cosa & Perché?
I numeri complessi sono un insieme di numeri della forma `a + bi`, dove `a` e `b` sono numeri reali, e `i` è l'unità immaginaria (`i^2 = -1`). In programmazione, li usiamo per risolvere problemi in vari domini, come l'ingegneria elettrica, l'elaborazione dei segnali e il calcolo quantistico.

## Come fare:
Python ha supporto integrato per i numeri complessi. Ecco come puoi utilizzarli:

```Python
# Creazione di numeri complessi
z = 4 + 5j
print(z)  # Output: (4+5j)

# Accesso alle parti reale e immaginaria
print(z.real)  # Output: 4.0
print(z.imag)  # Output: 5.0

# Aritmetica complessa
w = 1 - 2j
print(z + w)  # Output: (5+3j)
print(z - w)  # Output: (3+7j)
print(z * w)  # Output: (14+2j)
print(z / w)  # Output: (-3.6+1.2j)

# Modulo (valore assoluto)
print(abs(z))  # Output: 6.4031242374328485

# Congiugato di un numero complesso
print(z.conjugate())  # Output: (4-5j)
```

## Approfondimento
I numeri complessi furono concettualizzati per la prima volta da Gerolamo Cardano nel XVI secolo. Python, tra gli altri linguaggi di programmazione, tratta i numeri complessi come cittadini di prima classe. Questo significa che sono integrati nel linguaggio, con funzionalità facili da usare, evitando la necessità di importare librerie esterne per le operazioni di base.

Tuttavia, per calcoli numerici complessi, Python dispone di una libreria chiamata `cmath`, che è specifica per i numeri complessi. Ha funzioni aggiuntive come `exp`, `log` e operazioni trigonometriche.

Quando Python non è sufficiente, si potrebbe ricorrere a librerie come NumPy, specialmente per operazioni con array che coinvolgono numeri complessi. NumPy fornisce operazioni ottimizzate e vettorializzate che sono cruciali per le prestazioni nel calcolo numerico.

## Vedi Anche
Consulta queste risorse per saperne di più:

- Documentazione ufficiale di Python sui numeri complessi: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- Documentazione del modulo `cmath`: https://docs.python.org/3/library/cmath.html
- NumPy per gestire array di numeri complessi: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
