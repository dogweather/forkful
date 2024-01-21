---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:03:31.491218-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Uuden projektin aloittaminen on tyhjältä pöydältä alkaminen: uusi hakemisto, uusi Git-repo, uudet tiedostot. Koodarit aloittavat projekteja testatakseen ideoita, ratkaistakseen ongelmia tai vain opetellakseen jotain uutta.

## How to: - Näin teet:
```Fish Shell
# Luo uusi hakemisto projektillesi ja siirry sinne
mkdir minun_projektini
cd minun_projektini

# Alusta Git-repositorio
git init

# Luo esimerkiksi 'index.html' tiedosto
touch index.html

# Katso tiedostoja
ls
```
```
./minun_projektini
./minun_projektini/.git
./minun_projektini/index.html
```

## Deep Dive - Syväsukellus
Fish Shell, eli "friendly interactive shell", on vuonna 2005 syntynyt komentotulkki, joka keskittyi käyttäjäystävällisyyteen ja helppoon skriptattavuuteen. Vaihtoehtoja ovat Bash, Zsh ja monet muut, mutta Fish erottuu paremmalla automaattisella täydennyksellä ja selkeämmällä syntaksilla. Uuden projektin aloittaminen Fish Shellissä ei paljoa eroa muiden shellien käytöstä, mutta Fishin skriptaus ja mukauttaminen voivat olla sujuvampia.

## See Also - Katso Myös
- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Gitin aloitusopas](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)
- [Gitin ja Fish Shellin integrointi](https://github.com/jorgebucaran/fisher)