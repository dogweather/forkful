---
date: 2024-01-20 18:04:19.552143-07:00
description: "\xC5 starte et nytt prosjekt betyr \xE5 initialisere en ren arbeidsflate\
  \ for koding. Programmerere gj\xF8r dette for \xE5 f\xE5 en strukturert begynnelse\
  \ og unng\xE5 rot\u2026"
lastmod: 2024-02-19 22:05:00.601629
model: gpt-4-1106-preview
summary: "\xC5 starte et nytt prosjekt betyr \xE5 initialisere en ren arbeidsflate\
  \ for koding. Programmerere gj\xF8r dette for \xE5 f\xE5 en strukturert begynnelse\
  \ og unng\xE5 rot\u2026"
title: "\xC5 starte et nytt prosjekt"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å starte et nytt prosjekt betyr å initialisere en ren arbeidsflate for koding. Programmerere gjør dette for å få en strukturert begynnelse og unngå rot fra tidligere arbeid.

## Slik gjør du:
For å opprette et nytt Ruby-prosjekt, følg disse trinnene. Bruk terminalen for å opprette en ny mappe og initialiser et Git-repositorium for versjonskontroll:

```ruby
mkdir mitt_prosjekt
cd mitt_prosjekt
git init
```

Installer deretter nødvendige gems, som `bundler` for avhengighetsstyring:

```ruby
gem install bundler
bundler init
```

Dette vil opprette en `Gemfile` i prosjektet ditt. Du kan nå legge til gems som du trenger i `Gemfile` og installere dem:

```ruby
# Legg til i din Gemfile:
# gem "rails"

bundle install
```

Til slutt kan du opprette filer for koden din:

```ruby
touch app.rb
```

Skriver du nå i `app.rb`:

```ruby
puts "Hei, Norge!"
```

Kjøring av `ruby app.rb` i terminalen vil gi:

```ruby
Hei, Norge!
```

## Dypdykk
Før bundler ble vanlig brukte Ruby-programmerere `require` og `load` for å håndtere avhengigheter, men det var upraktisk med større applikasjoner. Bundler ble lansert rundt 2010 og løste mange av disse problemene ved å håndtere avhengigheter på en renere måte.

Alternativer til Bundler inkluderer RVMs gemsets, men disse blir ikke så ofte brukt nå som Bundler er standard. 

Når du starter et prosjekt, er det også vanlig å sette opp et `README.md` for dokumentasjon og `.gitignore` for å unngå unødvendige filer i Git-repositoriet.

## Se også

- Ruby's offisielle nettside for dokumentasjon: [Ruby-Docs](https://ruby-doc.org/)
- Bundler's offisielle nettside: [Bundler.io](https://bundler.io/)
- GitHub's .gitignore-templates: [github/gitignore](https://github.com/github/gitignore)
