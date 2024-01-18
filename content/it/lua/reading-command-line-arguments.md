---
title:                "Lettura degli argomenti da linea di comando"
html_title:           "Lua: Lettura degli argomenti da linea di comando"
simple_title:         "Lettura degli argomenti da linea di comando"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che cos'è e perché lo fanno i programmatori?

Leggere gli argomenti della riga di comando è un modo per consentire ai programmatori di passare informazioni al proprio programma durante l'esecuzione. Ad esempio, se si vuole che un programma calcoli la somma di due numeri inseriti dall'utente, è possibile passare questi numeri come argomenti della riga di comando.

## Come si fa:

```Lua
-- Questo programma calcola la somma di due numeri inseriti dall'utente

-- Leggi gli argomenti dalla riga di comando
local arg1 = tonumber(arg[1])
local arg2 = tonumber(arg[2])

-- Controlla che gli argomenti siano numeri
if not arg1 or not arg2 then
  print("Errore: inserisci due numeri")
else
  -- Calcola la somma
  local sum = arg1 + arg2
  print("La somma di", arg1, "e", arg2, "è:", sum)
end
```

Output se si esegue il programma con i due argomenti "10" e "5":
```
La somma di 10 e 5 è: 15
```

Output se si esegue il programma senza argomenti o con argomenti non numerici:
```
Errore: inserisci due numeri
```

## Approfondimento:

### Contesto storico:
La lettura dei parametri della riga di comando è un'operazione comune nei linguaggi di programmazione. Originariamente, questo meccanismo è stato utilizzato nei linguaggi a basso livello per passare informazioni al programma durante l'esecuzione tramite interrupt. Tuttavia, è ancora ampiamente utilizzato anche nei linguaggi moderni come Lua.

### Alternative:
In Lua, oltre a leggere gli argomenti dalla riga di comando, è possibile utilizzare la libreria `io` per ottenere l'input dell'utente durante l'esecuzione del programma. Ad esempio, il codice seguente otterrebbe lo stesso risultato del primo esempio:

```Lua
-- Questo programma calcola la somma di due numeri inseriti dall'utente

-- Leggi il primo numero
print("Inserisci il primo numero:")
local arg1 = tonumber(io.read())

-- Leggi il secondo numero
print("Inserisci il secondo numero:")
local arg2 = tonumber(io.read())

-- Controlla che gli argomenti siano numeri
if not arg1 or not arg2 then
  print("Errore: inserisci due numeri")
else
  -- Calcola la somma
  local sum = arg1 + arg2
  print("La somma di", arg1, "e", arg2, "è:", sum)
end
```

Tuttavia, utilizzare gli argomenti della riga di comando è spesso più conveniente, soprattutto per programmi più complessi.

### Dettagli di implementazione:
Nella maggior parte dei casi, gli argomenti della riga di comando vengono passati al programma in un array chiamato `arg`. In Lua, tuttavia, `arg` viene automaticamente shiftato di uno, quindi per accedere ai primi argomenti si utilizza `arg[1]`, `arg[2]`, e così via. Inoltre, per convertire gli argomenti in numeri, è necessario utilizzare la funzione `tonumber()`. Infine, è importante controllare che gli argomenti siano effettivamente numeri prima di eseguire operazioni matematiche su di essi per evitare errori durante l'esecuzione del programma.

## Vedi anche:

Link alla documentazione ufficiale di Lua sulle funzioni `tonumber()` e `io.read()`: https://www.lua.org/manual/5.4/manual.html#pdf-tonumber, https://www.lua.org/manual/5.4/manual.html#pdf-io.read