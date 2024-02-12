---
title:                "Avvio di un nuovo progetto"
aliases:
- /it/ruby/starting-a-new-project.md
date:                  2024-01-20T18:04:21.464213-07:00
model:                 gpt-4-1106-preview
simple_title:         "Avvio di un nuovo progetto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Avviare un nuovo progetto in Ruby significa creare una base da cui partire per sviluppare la tua applicazione. I programmatori lo fanno per ordinare il caos iniziale, impostare le best practices e accelerare lo sviluppo futuro.

## Come fare:
Per iniziare, installa Ruby se non l'hai ancora fatto. Poi usa `bundler` per gestire le gemme.

```Ruby
# Installa Bundler se non lo hai già
gem install bundler

# Inizializza un nuovo progetto Ruby
bundler init
```

Questo comanderà crea un file `Gemfile` base nel tuo progetto, pronto per personalizzarlo.

Aggiungi gemme al tuo `Gemfile`:

```Ruby
# Gemfile
source "https://rubygems.org"

gem "sinatra"
```

Esegui `bundle install` per installare le dipendenze:

```Ruby
bundle install
```

Crea il tuo primo script `app.rb`:

```Ruby
# app.rb
require 'sinatra'

get '/' do
  'Ciao Mondo!'
end
```

Esegui la tua app Sinatra:

```Ruby
ruby app.rb
```

Sample output:

`== Sinatra (v2.1.0) has taken the stage on 4567 for development with backup from Thin`

## Approfondimento
Ruby è rinomato per la sua capacità di rendere felici i programmatori. Lanciato nel 1995, ha reso popolare il framework Rails nel 2005, che ha influenzato il mondo dello sviluppo web con la sua filosofia "Convention over Configuration". Alternativamente, gemme come Sinatra permettono un approccio più minimalista e leggero. Per progetti più complexi, piattaforme come JRuby o Rubinius offrono performance migliorare e concorrenza.

## Vedi anche:
- [La documentazione ufficiale di Ruby](https://www.ruby-lang.org/it/documentation/)
- [Sinatra](http://sinatrarb.com/documentation.html)
- [Ruby on Rails Guides](https://guides.rubyonrails.org/)
- [Bundler](https://bundler.io/)
