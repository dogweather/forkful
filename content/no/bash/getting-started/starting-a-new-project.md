---
date: 2024-01-20 18:02:58.554797-07:00
description: "Hvordan: Start av nye prosjekter har endret seg mye. F\xF8r, ville man\
  \ kanskje bare \xE5pne en teksteditor og begynne \xE5 kode. N\xE5 legger vi vekt\
  \ p\xE5\u2026"
lastmod: '2024-04-05T22:50:54.977188-06:00'
model: gpt-4-1106-preview
summary: Start av nye prosjekter har endret seg mye.
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## Hvordan:
```Bash
# Opprett et nytt katalog for prosjektet ditt
mkdir my_new_project
cd my_new_project

# Initialiser Git for versjonskontroll
git init

# Opprett en README-fil for prosjektbeskrivelse
echo "# Mitt Nye Prosjekt" > README.md
git add README.md
git commit -m "Initial commit with README"

# Lag en ny oppstartsfiler for koding
touch main.sh
chmod +x main.sh
```
Output for `tree`:
```Bash
.
├── .git
└── README.md
└── main.sh
```

## Dypdykk
Start av nye prosjekter har endret seg mye. Før, ville man kanskje bare åpne en teksteditor og begynne å kode. Nå legger vi vekt på versjonskontroll fra start med verktøy som Git, og det settes ofte opp linter og automatiske tester tidlig. Det finnes alternativer til Git, som Mercurial eller SVN, men Git er bransjestandard. Et viktig aspekt ved oppsett er reproduserbarhet; verktøy som Docker kan være nyttige for å sikre at prosjektet ditt fungerer likt overalt.

## Se Også
- Git dokumentasjon: https://git-scm.com/doc
- Bash scripting tutorial: https://www.gnu.org/software/bash/manual/
- Docker's "Getting Started" guide: https://docs.docker.com/get-started/
