---
date: 2024-01-20 18:03:16.470353-07:00
description: "Slik gj\xF8r du: Start av et prosjekt er ofte samme sak uavhengig av\
  \ spr\xE5ket eller rammeverket. I Fish Shell har vi ikke innebygde\u2026"
lastmod: '2024-04-05T22:50:55.242261-06:00'
model: gpt-4-1106-preview
summary: "Start av et prosjekt er ofte samme sak uavhengig av spr\xE5ket eller rammeverket."
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## Slik gjør du:
```Fish Shell
# Sett opp en mappe for prosjektet
mkdir mitt-prosjekt
cd mitt-prosjekt

# Initialiser et Git-repositorium
git init

# Opprett en README.md for dokumentasjon
echo '# Mitt Prosjekt' > README.md
git add README.md
git commit -m "Legger til README"

# Installer avhengigheter (for eksempel med npm for Node.js-prosjekter)
npm init -y
npm install express --save

# Sjekk statusen for ditt Git-repositorium
git status
```
Output:
```
# ...utdata fra kommandoene over...
```

## Dykk dypere
Start av et prosjekt er ofte samme sak uavhengig av språket eller rammeverket. I Fish Shell har vi ikke innebygde prosjektadministrasjonsverktøy, men kommandolinjen gir friheten til å utføre nødvendige oppgaver. Historiesk har tradisjonelle Bash-skript vært normen, men Fish tilbyr en mer moderne og syntaktisk behagelig tilnærming. Alternative skall inkluderer Zsh og den klassiske Bash, blant andre. Fish skiller seg ut med autokomplettering og enklere syntaks, som forenkler prosessen med å sette opp prosjekter.

## Se også
- Fish Shell dokumentasjon: https://fishshell.com/docs/current/index.html
- GitHub's guide til Markdown: https://guides.github.com/features/mastering-markdown/
- npm dokumentasjon for å håndtere pakker: https://docs.npmjs.com/cli/init
