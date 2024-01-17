---
title:                "Utmatning av felsökningsdata"
html_title:           "Python: Utmatning av felsökningsdata"
simple_title:         "Utmatning av felsökningsdata"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

När vi programmerar, är det inte alltid att allt går som vi vill. Ibland behöver vi veta vad som händer under programmets körning, för att förstå varför det inte fungerar som det ska. Det är där "debug output" kommer in - det är ett sätt att skriva ut information under programmets körning för att följa vad som händer.

## Hur gör man?

För att skriva ut debug output i Python använder vi funktionen "print()". Det finns många olika sätt att använda print() på, beroende på vad vi vill uppnå. Här är några exempel:

```python
x = 5
y = "Hello"
print(x) # skriver ut värdet av x på skärmen - i detta fall 5
print(y) # skriver ut värdet av y på skärmen - i detta fall "Hello"
print("Detta är ett meddelande") # skriver ut en textsträng på skärmen
```

Output skulle se ut såhär:

```
5
Hello
Detta är ett meddelande
```

Vi kan också kombinera flera värden i samma print() funktion genom att använda "strängformatering". Detta gör vi genom att sätta in variabler eller värden i en textsträng med hjälp av "%s". Här är ett exempel:

```python
name = "Annie"
age = 25
print("Mitt namn är %s och jag är %d år gammal." % (name, age)) # skriver ut en textsträng med värden av variablerna name och age insatta
```

Output skulle se ut såhär:

```
Mitt namn är Annie och jag är 25 år gammal.
```

## Djupdykning

Debug output har funnits sedan början av datorer. Ursprungligen användes det genom att utskrifter skickades till en skrivare, men nu för tiden används det oftast genom att skriva ut på skärmen.

Alternativet till att använda print() är att använda en "debugger", vilket är ett speciellt program som låter oss undersöka variabler och steg för steg följa vad som händer i programmet. Detta är speciellt användbart när problemet är svårt att hitta eller om vi vill undersöka mer specifikt vad som händer i programmet.

Om vi vill skriva ut mer avancerad information, som t.ex. objekt av en viss typ eller information från en specifik rad i koden, kan vi använda "logging" modulen i Python. Detta ger oss mer möjligheter och kontroll över vad vi vill skriva ut och var vi vill skriva ut det.

## Se även

För mer information om print() funktionen och hur man använder den kan du kolla in den officiella Python dokumentationen här: https://docs.python.org/3/library/functions.html#print

För mer information om "logging" modulen kan du kolla in den officiella Python dokumentationen här: https://docs.python.org/3/library/logging.html