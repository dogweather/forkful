---
date: 2024-01-20 17:57:10.723284-07:00
description: "Att l\xE4sa in kommandoradsargument \xE4r metoden att f\xE5nga anv\xE4\
  ndarinput fr\xE5n konsolen i ditt program. Programmerare g\xF6r det f\xF6r att till\xE5\
  ta anv\xE4ndare att\u2026"
lastmod: '2024-03-11T00:14:11.009773-06:00'
model: gpt-4-1106-preview
summary: "Att l\xE4sa in kommandoradsargument \xE4r metoden att f\xE5nga anv\xE4ndarinput\
  \ fr\xE5n konsolen i ditt program. Programmerare g\xF6r det f\xF6r att till\xE5\
  ta anv\xE4ndare att\u2026"
title: "L\xE4sa in kommandoradsargument"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa in kommandoradsargument är metoden att fånga användarinput från konsolen i ditt program. Programmerare gör det för att tillåta användare att skräddarsy exekveringen av applikationen genom att ge olika värden vid start.

## Hur gör man:
Dags att dyka in. För att börja, installera `ts-node` för att kunna köra TypeScript filer direkt:

```bash
npm install -g ts-node
```

Skapa sen en `parseArgs.ts` och använd process-objektet:

```TypeScript
// parseArgs.ts

// Argumenten finns i 'process.argv', index 2 och framåt
const args = process.argv.slice(2);

console.log(args);
```

Kör filen såhär:

```bash
ts-node parseArgs.ts arg1 arg2 arg3
```

Förväntat svar skulle vara:

```plaintext
[arg1, arg2, arg3]
```

## Djupdykning:
Kommandoradsargument i Node.js (och TypeScript) hanteras genom globala `process`-objektet, specifikt `argv` egenskapen. Historiskt sett härstammar detta från C och Unix-världen där argumenten hanterades genom `argv[]` i `main`-funktionen.

Det finns alternativ som `commander` och `yargs` för mer avancerad manipulering och validering av kommandoradsargument.

Implementationen är omedelbar: indexerade från 0 där index 0 är sökvägen till node.exe och index 1 är den exekverade .ts filens sökväg. Därför börjar våra egna argument från index 2.

## Se också:
- Node.js dokumentation om process.argv: https://nodejs.org/docs/latest-v16.x/api/process.html#process_process_argv
- `commander` npm paket: https://www.npmjs.com/package/commander
- `yargs` npm paket: https://www.npmjs.com/package/yargs
