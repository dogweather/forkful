---
date: 2024-01-20 17:30:52.701163-07:00
description: 'How to: - Comment faire : .'
lastmod: '2024-04-05T21:53:59.738207-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## How to: - Comment faire :
```Fish Shell
# Ajouter 10 jours à aujourd'hui
set future_date (date -d "+10 days" +"%Y-%m-%d")
echo $future_date

# Sortie attendue: 2023-04-21 (dépend de la date actuelle)

# Soustraire 20 jours à partir d'aujourd'hui
set past_date (date -d "-20 days" +"%Y-%m-%d")
echo $past_date

# Sortie attendue: 2023-03-22 (dépend de la date actuelle)
```

## Deep Dive - Plongée en profondeur
L'histoire de la gestion des dates dans les langages de programmation est pleine de pièges, surtout à cause des fuseaux horaires et des calendriers différents. En Fish Shell, on utilise souvent `date`, une commande Unix, car elle est puissante et flexible. Attention aux systèmes où `date` se comporte différemment (comme macOS). Des alternatives? Des langages comme Python ou PHP avec leurs propres bibliothèques de dates. Pour l'implémentation, on manie avec soin les formats et les calculs pour éviter des erreurs subtiles comme des débordements d'heure ou de date.

## See Also - Voir Aussi
- La page de manuel de `date`: https://man7.org/linux/man-pages/man1/date.1.html
- Gestion des dates en Python: https://docs.python.org/3/library/datetime.html
- Gestion des dates en PHP: https://www.php.net/manual/en/book.datetime.php
