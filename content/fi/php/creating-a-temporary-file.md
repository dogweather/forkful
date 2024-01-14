---
title:                "PHP: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto?

Väliaikaiset tiedostot ovat välttämättömiä monissa PHP-ohjelmoinnin sovelluksissa, kuten latausten käsittelyssä ja tietojen tallentamisessa. Luodessaan väliaikaisen tiedoston, voit varmistaa, että tiedot ovat turvallisesti tallennettuina ja että ne eivät häviä, vaikka ohjelma kaatuu tai istunto päättyy. 

## Miten luoda väliaikainen tiedosto?

Luodaksesi väliaikaisen tiedoston PHP:ssa, käytä funktiota `tmpfile()`. Tämä funktio luo väliaikaisen tiedoston, jonka avulla voit tallentaa väliaikaisia tietoja ja käyttää niitä myöhemmin. Seuraava esimerkki näyttää, kuinka luodaan väliaikainen tiedosto ja tallennetaan siihen merkkijono "Hello world!".

```PHP
$file = tmpfile();
fwrite($file, "Hello world!");
echo fgets($file);
```

Tulostus:

```
Hello world!
```

## Syvällisemmin väliaikaisista tiedostoista

Väliaikaiset tiedostot luodaan yleensä käyttämällä palvelimen oletusvälimuistia, mutta voit myös asettaa väliaikaisten tiedostojen tallennuspaikan manuaalisesti `sys_get_temp_dir()` -funktion avulla. Lisäksi voit poistaa väliaikaiset tiedostot käytöstä funktiolla `unlink()`, joka poistaa tiedoston heti sen käytön jälkeen.

# Katso myös

- [PHP.net - tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP.net - sys_get_temp_dir()](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
- [PHP.net - unlink()](https://www.php.net/manual/en/function.unlink.php)