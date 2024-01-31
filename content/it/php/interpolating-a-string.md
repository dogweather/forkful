---
title:                "Interpolazione di una stringa"
date:                  2024-01-20T17:51:24.527509-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolazione di una stringa"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
L'interpolazione di stringhe consente di incastrare variabili direttamente dentro una stringa. Facilita la lettura e la scrittura del codice, rendendolo più pulito e semplificando la concatenazione delle stringhe.

## How to: (Come fare)
```PHP
<?php
$planet = "Terra";
$greeting = "Benvenuti sul pianeta $planet!"; // Interpolazione diretta

echo $greeting; // Output: Benvenuti sul pianeta Terra!

// Usando le parentesi graffe
$exclamation = "fantastico";
echo "Che giorno {$exclamation}o!"; // Output: Che giorno fantastico!
?>
```

## Deep Dive (Approfondimento)
L'interpolazione di stringhe è presente in PHP fin dalle prime versioni e contribuisce a rendere il codice più leggibile. Prima dell'avvento di questa funzionalità, la concatenazione era l'unica opzione, cosa che rendeva il codice più verboso e difficile da seguire. Quindi, l'interpolazione è stata una benedizione.

Alternativamente, PHP offre la concatenazione con l'operatore `.` ma, soprattutto in stringhe complesse, l'interpolazione risulta più snella e diretta.

Implementare l'interpolazione di stringhe richiede l'uso di doppi apici `"`; se si usano apici singoli `'`, PHP tratterà tutto come testo letterale. Attenzione, inoltre, se necessario, a racchiudere la variabile tra graffe `{}` per evitare ambiguità nel codice.

## See Also (Vedi Anche)
- [La documentazione ufficiale su PHP.net a proposito delle stringhe](https://www.php.net/manual/en/language.types.string.php)
- [sprintf(): Una funzione utile per la formattazione di stringhe](https://www.php.net/manual/en/function.sprintf.php)
- [HEREDOC e NOWDOC: Altri modi per creare stringhe complesse](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc)
