---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:18:43.910401-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"

category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En Les-Evaluer-Skriv-Løkke (REPL) er et programmeringsmiljø som tar enkeltbrukerinput, utfører dem og returnerer resultatet til brukeren. Programmerere bruker en REPL for å raskt eksperimentere med kodesnutter, feilsøke, og lære nye språkegenskaper uten overhead av å skape en fullstendig applikasjon.

## Hvordan:
TypeScript kommer ikke med sin egen REPL. La oss bruke `ts-node`, et TypeScript kjøremiljø for Node.js som inkluderer en REPL.

Først, installer den globalt:
```bash
npm install -g ts-node
```

Start REPL ved å skrive `ts-node` i din kommandolinje:
```bash
ts-node
```

Her er et raskt snutt å prøve:
```TypeScript
> let melding: string = 'Hei, REPL!';
> console.log(melding);
Hei, REPL!
> 
```
For å avslutte økten, trykk `Ctrl+D`.

## Dypdykk
Historisk sett var REPLs fremtredende i språk som Lisp, som tillot dynamisk kodeevaluering. Konseptet har siden spredt seg og blitt en grunnleggende del for interaktiv koding i mange språk.

For TypeScript er `ts-node` ikke ditt eneste alternativ. Alternativer inkluderer å bruke TypeScript Playground i en nettleser eller å bruke andre Node.js-baserte REPLs som støtter TypeScript med passende plugins.

Når det gjelder implementering, bruker `ts-node` TypeScript-kompiler-APIen for å transpilere kode på-flyt før den blir utført av Node.js. Dette gir deg umiddelbar tilbakemelding og er spesielt nyttig for å prøve ut TypeScripts nyeste funksjoner uten oppsettsproblemer.

Én ting å huske på – mens en REPL er flott for raske tester, erstatter den ikke å skrive tradisjonell, testbar og vedlikeholdbar kode. Det er et verktøy for læring og utforskning, ikke et substitutt for ordentlige utviklingspraksiser.

## Se Også
- [TypeScript Offisiell Nettside](https://www.typescriptlang.org/)
- [ts-node på GitHub](https://github.com/TypeStrong/ts-node)
- [Node.js REPL Dokumentasjon](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)
