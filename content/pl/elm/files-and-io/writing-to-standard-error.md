---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:09.547436-07:00
description: "Jak to zrobi\u0107: Elm jest przede wszystkim ukierunkowany na rozw\xF3\
  j aplikacji internetowych, gdzie koncepcja bezpo\u015Bredniego pisania do stderr\
  \ nie ma\u2026"
lastmod: '2024-03-13T22:44:35.338198-06:00'
model: gpt-4-0125-preview
summary: "Elm jest przede wszystkim ukierunkowany na rozw\xF3j aplikacji internetowych,\
  \ gdzie koncepcja bezpo\u015Bredniego pisania do stderr nie ma zastosowania w taki\
  \ spos\xF3b, jak ma to miejsce w tradycyjnych \u015Brodowiskach linii polece\u0144\
  ."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:
Elm jest przede wszystkim ukierunkowany na rozwój aplikacji internetowych, gdzie koncepcja bezpośredniego pisania do stderr nie ma zastosowania w taki sposób, jak ma to miejsce w tradycyjnych środowiskach linii poleceń. Jednakże, dla programów Elm działających w Node.js lub podobnych środowiskach, współdziałanie z JavaScriptem za pomocą portów jest kluczowym podejściem do osiągnięcia podobnej funkcjonalności. Oto jak można to ustawić:

Kod Elm (`Main.elm`):
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- Przykładowa funkcja-zabawka, która wysyła komunikat o błędzie do JS
generateError : String -> Cmd msg
generateError message =
    errorOut message

main =
    generateError "To jest komunikat o błędzie dla stderr"
```

Interop JavaScript (`index.js`):
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```

Ten kod Elm definiuje port `errorOut`, który umożliwia wysyłanie wiadomości z Elm do JavaScriptu. Następnie w kodzie JavaScript nasłuchujemy wiadomości wysyłanych przez ten port i przekierowujemy je do stderr za pomocą `console.error()`. W ten sposób, możesz skutecznie pisać do stderr w środowisku, które to wspiera, wykorzystując funkcje interop Elm z JavaScriptem.

Przykładowe wyjście w terminalu Node.js (po uruchomieniu `index.js`):
```
To jest komunikat o błędzie dla stderr
```
