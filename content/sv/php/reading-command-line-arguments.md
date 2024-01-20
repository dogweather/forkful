---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# PHP-programmering: Läsa Kommandoradsargument

## Vad och Varför?
Vi talar om att läsa kommandoradsargument i PHP. Det handlar om att gripa och använda värden som anges när ett PHP-skript körs från terminalen. Vi gör det för att möjliggöra flexibla skript som kan hantera olika scenarier baserade på angivna värden.

## Hur:
PHP har ett inbyggt superglobalt array `$_SERVER` som hjälper oss att läsa kommandoradsargument. Det första elementet av `$_SERVER['argv']` representerar filnamnet. Följande element representerar argumenten.

```PHP
#!/usr/bin/php
<?php
print_r($_SERVER['argv']);
?>
```

Om vi sparar skriptet ovan som `script.php` och kör det från kommandoraden:

```bash
$ php script.php arg1 arg2 arg3
```

Output:

```bash
Array
(
    [0] => script.php
    [1] => arg1
    [2] => arg2
    [3] => arg3
)
```

## Fördjupning
Från PHP 4.3.0, är `register_argc_argv`-direktivet tillgängligt. Om den är inställd till "Off" skulle `$_SERVER['argv']` inte vara tillgänglig. Se till att den är "On" i php.ini. Alternativt, kan du få tillgång till kommandoradsargument via `$argv` och `$argc` som alltid är tillgängliga oavsett `register_argc_argv`-direktivet.

PHP följer Unix-konventionen där argumentindex börjar med 0. Argument 0 är alltid skriptfilnamnet, följt av de valfria argumenten i antalet ordning.

## Se Även
1. PHP Manual: [$_SERVER](https://www.php.net/manual/en/reserved.variables.server.php)
2. PHP Manual: [register_argc_argv directive](https://www.php.net/manual/en/ini.core.php#ini.register-argc-argv)
3. PHP Manual: [$argv and $argc](https://www.php.net/manual/en/reserved.variables.argv.php)