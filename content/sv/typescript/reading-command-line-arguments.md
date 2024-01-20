---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsa kommandoradsargumenten är processen att extrahera och tolka de data som skickas till ett program vid körning. Programmerare gör detta för att manipulera programmets beteende beroende på användarens önskemål.

## Hur gör man:
Med Node.js kan du läsa kommandoradsargument med `process.argv`, ett inbyggt objekt som returnerar en array av kommandoradsargument.

```TypeScript
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});
```

Om du kör detta skript med Node.js och lägger till några argument (till exempel `node script.js arg1 arg2`), kommer du att se följande utskrift:

```bash
0: /path/to/node
1: /path/to/script.js
2: arg1
3: arg2
```

Notera att `process.argv` alltid har minst två element.

## Djupdykning
Läsning av kommandoradsargument har sin base i tidigare programparadigm, där interaktivt input var begränsat eller icke-existerande. Till exempel i C, skulle du genom att definiera `main` funktionen med parametrar `(int argc, char *argv[])` kan läsa kommandoradsargument.

Som alternativ kan vi i Node.js använda paket som `minimist` eller `yargs` för att underlätta hanteringen och tolkningen av kommandoradsargument.

Intressant nog, `process.argv` är inte en ren array. Det är faktiskt en instans av `Object` med några Array-liknande egenskaper. 

## Se också
- MDN-dokumentation om 'process.argv' (https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- Paketet `minimist` på npm (https://www.npmjs.com/package/minimist)
- Paketet `yargs` på npm (https://www.npmjs.com/package/yargs)