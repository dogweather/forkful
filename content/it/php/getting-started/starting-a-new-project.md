---
date: 2024-01-20 18:03:58.829742-07:00
description: "Iniziare un nuovo progetto PHP significa mettere su tela bianca le basi\
  \ del tuo codice. I programmatori lo fanno per dare vita a nuove idee e risolvere\u2026"
lastmod: '2024-03-11T00:14:17.114966-06:00'
model: gpt-4-1106-preview
summary: "Iniziare un nuovo progetto PHP significa mettere su tela bianca le basi\
  \ del tuo codice. I programmatori lo fanno per dare vita a nuove idee e risolvere\u2026"
title: Avvio di un nuovo progetto
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Iniziare un nuovo progetto PHP significa mettere su tela bianca le basi del tuo codice. I programmatori lo fanno per dare vita a nuove idee e risolvere problemi con soluzioni su misura.

## How to: (Come fare:)
Per iniziare, installa il PHP, usa Composer per gestire le dipendenze e crea il tuo primo script. Ecco i passi chiave:

```PHP
<?php
// Installa PHP e Composer, poi inizializza un nuovo progetto
composer init

// Crea un file 'index.php'
echo "<?php phpinfo(); ?>" > index.php

// Esegui lo script dal server locale
php -S localhost:8000
```

Ora visita `localhost:8000` nel tuo browser per vedere le informazioni di PHP.

## Deep Dive (Approfondimento)
PHP è nato nel 1995 e da allora è maturato come linguaggio lato server per il web. Inizialmente, era un semplice strumento per mantenere pagine personali, ma si è evoluto in uno strumento robusto per costruire applicazioni complesse. 

I moderni framework PHP come Laravel, Symfony e CakePHP offrono approcci alternativi per iniziare un progetto, semplificandone l'implementazione e la manutenzione. Usano MVC (Model-View-Controller) per organizzare il codice e Composer per la gestione delle dipendenze, affermandosi come pratiche standard nello sviluppo PHP.

Alcuni sviluppatori preferiscono microframework come Slim e Lumen per progetti più piccoli o servizi API. Questi framework sono leggeri, ma offrono molte delle stesse funzionalità dei loro cugini più grandi.

## See Also (Vedi Anche)
- [Documentazione ufficiale di PHP](https://www.php.net/manual/en/)
- [Composer, il gestore di dipendenze per PHP](https://getcomposer.org/)
- [Laravel, un framework PHP per web artisans](https://laravel.com/)
- [Symfony, un set di componenti PHP e un framework](https://symfony.com/)
- [CakePHP, un framework rapido per lo sviluppo](https://cakephp.org/)
- [Slim, un microframework per PHP](https://www.slimframework.com/)
- [Lumen, un microframework PHP per servizi web leggeri](https://lumen.laravel.com/)
