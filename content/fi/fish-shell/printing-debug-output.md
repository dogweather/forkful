---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tulostaminen debug-tulostus on prosessi, jonka avulla ohjelmoijat voivat seurata ja ymmärtää koodin käyttäytymistä sen suorituksen aikana. Sitä käytetään sen varmistamiseen, että koodi toimii odotetusti ja löytämään mahdolliset virheet tai puutteet.

## Näin se tehdään:
Fish Shell tarjoaa `echo` komennon, jonka avulla voit tulostaa debug-tiedot. 

```fish
for i in (seq 5)
    echo "Processing loop iteration $i"
end
```
Tämä tuottaa seuraavanlähteen:

```fish
Processing loop iteration 1
Processing loop iteration 2
Processing loop iteration 3
Processing loop iteration 4
Processing loop iteration 5
```
## Syvä sukellus
Debug-tulostuksen käyttö on ollut kehitystyökalujen peruskäytäntö alusta asti. Fish Shell on toteutettu läpinäkyvyyden puitteissa, ja se tarjoaa joukon erilaisia keinoja debuglogituksen suorittamiseksi, mukaan lukien `stdlib`, `debug`, ja `status` static functionit. Vaikka `echo` on yleisin tapa, voit myös käyttää `printf` tuottaaksesi formatoidumman ulostulon.

## Katso myös
Lisätietoja Fish Shell -ohjelmoinnista ja debug -tulostuksesta voit löytää seuraavasta lähteistä:
1. [Fish Shell viralliset dokumentit](https://fishshell.com/docs/current/index.html)
2. [Fish Shell GitHub](https://github.com/fish-shell/fish-shell)
3. [Stack Overflow: How to debug in Fish Shell](https://stackoverflow.com/questions/32764976/how-do-i-debug-fish-shell-scripts)
4. [Debugging Fish Shell Scripts](https://opensource.com/article/19/8/debugging-fish-shell)