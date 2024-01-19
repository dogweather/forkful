---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/extracting-substrings.md"
---

{{< edit_this_page >}}

# Vad & varför?
Att extrahera delstränger innebär att plocka ut en specifik del av en sträng baserad på givna positioner. Programmerare gör detta för att behandla särskilda delar av data, exempelvis för att samla emails från text eller specifika datum från en loggfil.

# Så här gör man:
I PHP kan man använda flera funktioner för att extrahera delstränger, däribland substr() och strstr(). Här är några exempel:

Hämta de första fyra tecknen från en sträng:

```PHP
<?php
$string = 'Programmering i PHP';
$substring = substr($string, 0, 4);
echo $substring;
?>
```
Output: `Prog` 

Strstr() returnerar allt efter det första uppträdandet av ett visst karaktär eller ord:

```PHP
<?php
$string = 'Programmering i PHP';
$substring = strstr($string, 'i');
echo $substring;
?>
```
Output: `i PHP`

# Djupdykning:

Historiska sammanhang: PHP's substr() och strstr() funktioner har funnits med sedan de första versionerna av PHP. Dessa tillhandahåller grunden för att manipulera strängar i PHP.

Alternativ: Bortsett från substr() och strstr() funktionerna, erbjuder PHP också en funktion som kallas substr_count() för att hitta antalet gånger en substring förekommer inom en sträng.

Funktionsdetaljer: substr() och strstr() är båda case-sensitive. Om du behöver icke-känsliga alternativ, överväg användning av strtolower() eller strtoupper() för att konvertera strängar till gemena eller versala bokstäver respektive innan extraktion.

# Se även: 

För mer information om hantering av strängar i PHP, se följande länkar:

- PHP Manual: substr() - http://www.php.net/manual/en/function.substr.php
- PHP Manual: strstr() - http://www.php.net/manual/en/function.strstr.php 
- PHP Manual: substr_count() - http://php.net/manual/en/function.substr-count.php 
- PHP Manual: strtolower() - http://php.net/manual/en/function.strtolower.php 
- PHP Manual: strtoupper() - http://php.net/manual/en/function.strtoupper.php