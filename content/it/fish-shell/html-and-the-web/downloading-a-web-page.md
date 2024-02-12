---
title:                "Scaricare una pagina web"
aliases: - /it/fish-shell/downloading-a-web-page.md
date:                  2024-01-20T17:44:06.562048-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Scaricare una pagina web significa copiare il suo contenuto HTML localmente attraverso internet. I programmatori lo fanno per analisi dati, testare siti, o automatizzare compiti web.

## How to:
Per scaricare una pagina web in Fish, puoi usare `curl` o `wget`. Ecco gli esempi:

```Fish Shell
# Usando curl
curl https://example.com -o mypage.html

# Usando wget
wget https://example.com -O mypage.html
```

Risultati esemplificativi:

```
# Risultato di curl
% curl https://example.com -o mypage.html
% cat mypage.html
<!doctype html>...
```

```
# Risultato di wget
% wget https://example.com -O mypage.html
% cat mypage.html
<!doctype html>...
```

## Deep Dive
I comandi `curl` e `wget` sono utilità classiche di UNIX per il trasferimento dati da o verso un server. Presenti dai primi tempi di internet, questi strumenti sono diventati standard per le operazioni di rete. `curl` supporta molte librerie e protocolli, rendendolo versatile per scopi diversi. `wget` è ottimizzato per il download non interattivo, come scaricare ricorsivamente tutto un sito web. Mentre `curl` è più focalizzato sul singolo trasferimento di file, `wget` può seguire i link e formare strutture di cartelle locali basate sul sito originale.

## See Also
- Documentazione Fish Shell: https://fishshell.com/docs/current/index.html
- Guida a `curl`: https://curl.se/docs/manpage.html
- Manuale di `wget`: https://www.gnu.org/software/wget/manual/wget.html
- Confronto tra `wget` e `curl`: https://daniel.haxx.se/docs/curl-vs-wget.html
