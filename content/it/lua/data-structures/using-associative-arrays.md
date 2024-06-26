---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:44.433916-07:00
description: "Come fare: In Lua, creare un array associativo (o una tabella, in termini\
  \ Lua) \xE8 semplice. Si abbandonano gli indici numerici usuali per chiavi di propria\u2026"
lastmod: '2024-03-13T22:44:43.551086-06:00'
model: gpt-4-0125-preview
summary: "In Lua, creare un array associativo (o una tabella, in termini Lua) \xE8\
  \ semplice."
title: Utilizzo di array associativi
weight: 15
---

## Come fare:
In Lua, creare un array associativo (o una tabella, in termini Lua) è semplice. Si abbandonano gli indici numerici usuali per chiavi di propria scelta. Guarda qui:

```Lua
-- Creazione di un array associativo
userInfo = {
  name = "Jamie",
  occupation = "Avventuriero",
  level = 42
}

-- Accesso agli elementi
print(userInfo["name"]) -- Stampa Jamie
print(userInfo.occupation) -- Stampa Avventuriero

-- Aggiungere nuove coppie chiave-valore
userInfo["hobby"] = "Programmazione"
userInfo.favLang = "Lua"

-- Iterazione sull'array associativo
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

Output:
```
Jamie
Avventuriero
name: Jamie
occupation: Avventuriero
level: 42
hobby: Programmazione
favLang: Lua
```

La parte interessante? Interagisci con i dati usando chiavi significative per te, rendendo il codice più leggibile e mantenibile.

## Approfondimento
Quando Lua è entrato in scena, ha introdotto le tabelle come una struttura dati universale, rivoluzionando il modo in cui gli sviluppatori gestiscono i dati. A differenza di alcuni linguaggi in cui gli array associativi e gli array sono entità distinte, le tabelle di Lua fungono sia da uno che dall'altro, semplificando il panorama delle strutture dati.

Ciò che rende le tabelle Lua particolarmente potenti è la loro flessibilità. Tuttavia, questa flessibilità comporta potenziali implicazioni sulla performance, specialmente con grandi dataset dove una struttura dati più specializzata potrebbe essere preferibile per l'efficienza.

Anche se Lua non supporta nativamente strutture dati più convenzionali fuori dalla scatola, come liste collegate o mappe hash, l'adattabilità delle strutture tabellari significa che puoi implementarle usando tabelle se necessario. Ricorda solo: con grande potere viene grande responsabilità. Usa la flessibilità con saggezza per mantenere performance e leggibilità del tuo codice.
