---
title:    "Elixir: Ett nytt projekt påbörjas"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt projekt i Elixir kan vara en spännande utmaning för dig som programmerare. Genom att använda funktionell programmering och koncept som "actor concurrency" kan du skapa skalbara och robusta applikationer.

## Så här gör du

För att starta ett nytt projekt i Elixir behöver du först installera Elixir och dess tillhörande pakethanterare Hex. Sedan kan du följa dessa steg för att skapa ett nytt projekt:

```Elixir
# Skapa en ny mapp för ditt projekt
mkdir min_awesome_app
cd min_awesome_app

# Initiera en ny Elixir applikation
mix new min_awesome_app

# Gå in i projektmappen
cd min_awesome_app

# Starta Elixir interaktiv miljö
iex -S mix
```

Om du nu skriver `HelloWorld.hello()`, så bör du få ut "Hello world!" som output.

## Djupdykning

När du har skapat ditt projekt kan du börja leka runt med Elixirs syntax och skapa dina egna funktioner och moduler. Här är ett grundläggande exempel på hur du kan definiera en klass och ett objekt i Elixir:

```Elixir
defmodule Person do
  def new(name) do
    %{name: name}
  end
end

person = Person.new("John")
person.name # Skriver ut "John"
```

Genom att förstå koncept som "pattern matching" och "pipe operator" kan du skapa effektiva och eleganta lösningar i Elixir. Studera även hur "supervisors" fungerar för att hantera fel och hålla din applikation igång.

## Se även

- Officiell Elixir hemsida: https://elixir-lang.org/
- Elixir School (på svenska): https://elixirschool.com/sv/
- Awesome Elixir lista med användbara bibliotek och verktyg: https://github.com/h4cc/awesome-elixir