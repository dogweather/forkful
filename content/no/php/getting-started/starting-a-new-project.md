---
date: 2024-01-20 18:04:10.370550-07:00
description: "\xC5 starte et nytt prosjekt er som \xE5 slippe l\xF8s kreativiteten\
  \ din med kode som lerret. Programmerere gj\xF8r det for \xE5 l\xF8se problemer,\
  \ utforske nye ideer,\u2026"
lastmod: '2024-03-13T22:44:40.887108-06:00'
model: gpt-4-1106-preview
summary: "\xC5 starte et nytt prosjekt er som \xE5 slippe l\xF8s kreativiteten din\
  \ med kode som lerret."
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## How to: (Hvordan)
La oss lage en enkel PHP-fil som sier "Hei, verden!". Opprett en fil kalt `hello_world.php`.

```PHP
<?php
echo "Hei, verden!";
?>
```

Hvis du kjører dette i nettleseren eller en PHP server, får du:

```
Hei, verden!
```

Neste, legg til en variabel og en funksjon. Koden nedenfor sier hei til et navn som er gitt som en parameter.

```PHP
<?php
$name = "Ola Nordmann";

function sayHello($name) {
    echo "Hei, " . $name . "!";
}

sayHello($name);
?>
```

Kjører du den, ser du:

```
Hei, Ola Nordmann!
```

## Deep Dive (Dypdykk)
I gamle dager, var PHP-script ofte blandet med HTML. Med tidens gang har PHP utviklet seg til å støtte moderne best practices, inkludert objektorientert programmering og bruk av komponist for pakkehåndtering.

Det finnes alternativer for å starte prosjekter, som å bruke rammeverk som Laravel eller Symfony. Disse rammeverkene gir en strukturert måte å bygge applikasjoner på og kommer med tilleggsfunksjoner som databasetilkobling og autentisering.

Når du starter et nytt prosjekt, må du først bestemme om du vil ha et lettvekts script eller en robust applikasjon. Deretter setter du opp en utviklingsmiljø – kan være lokalt på PC-en din eller i skyen. For å få prosjektet på rett spor, bruk versjonskontroll med Git fra starten.

## See Also (Se Også)
- PHP's offisielle nettside: [php.net](https://www.php.net/)
- Lær mer om Composer: [getcomposer.org](https://getcomposer.org/)
- Introduksjon til objektorientert PHP: [phptherightway.com](https://phptherightway.com/#object-oriented-programming)
- Begynn med Laravel: [laravel.com](https://laravel.com/)
- Utforsk Symfony: [symfony.com](https://symfony.com/)
