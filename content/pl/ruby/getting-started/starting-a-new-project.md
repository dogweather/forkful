---
title:                "Rozpoczynanie nowego projektu"
aliases:
- /pl/ruby/starting-a-new-project/
date:                  2024-01-20T18:04:15.206156-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rozpoczynanie nowego projektu"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Rozpoczęcie nowego projektu programistycznego to jak otwarcie pustej księgi i decydowanie, co będzie w niej napisane. Programiści rozpoczynają nowe projekty, by tworzyć oprogramowanie spełniające określone potrzeby lub realizować własne pomysły.

## Jak to zrobić:

Stwórzmy nowy projekt w Ruby za pomocą Bundler:

```Ruby
# Zainstaluj Bundler, jeśli jeszcze tego nie zrobiłeś
gem install bundler

# Utwórz nowy projekt
bundler init

# Dodaj do Gemfile potrzebne gem'y
gem 'sinatra'
```

Wypełnij `Gemfile` i zainstaluj zależności:

```Ruby
# Zainstaluj gem'y zdefiniowane w Gemfile
bundle install
```

Teraz możesz zacząć budować swoją aplikację. Stwórz plik `app.rb`:

```Ruby
# app.rb
require 'sinatra'

get '/' do
  "Witaj w mojej nowej apce!"
end
```

Uruchom aplikację:

```Ruby
ruby app.rb
```

I sprawdź w przeglądarce wynik:

```
http://localhost:4567/
```

Powinno pojawić się "Witaj w mojej nowej apce!".

## Głębsze spojrzenie

Kiedyś, tworzenie nowego projektu zaczynało się od otwarcia pustego pliku tekstowego. Dziś jest o wiele łatwiej dzięki narzędziom jak RubyGems i Bundler, które zarządzają zależnościami. Historia Rubiego, którego pierwsza wersja pojawiła się w 1995 roku, pełna jest przykładów, jak narzędzia te ułatwiły programistom życie.

Alternatywą dla Bundlera jest używanie czystego RubyGems lub Rbenv do zarządzania gemami, ale Bundler zapewnia większą kontrolę nad zależnościami w projekcie.

Przy implementacji projektu warto pamiętać, by zachowywać strukturę katalogów czystą i uporządkowaną. Stosowanie `bundler` pozwala na utrzymanie spójnych środowisk między różnymi maszynami i developerami - to bardzo ważne, zwłaszcza przy pracy zespołowej.

## Zobacz również

- [RubyGems user guide](https://guides.rubygems.org/)
- [Bundler documentation](https://bundler.io/docs.html)
- [Sinatra](http://sinatrarb.com/)
- [Rbenv](https://github.com/rbenv/rbenv)
- [Oficjalna strona Rubiego](https://www.ruby-lang.org/pl/)
