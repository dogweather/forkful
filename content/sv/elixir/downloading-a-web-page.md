---
title:                "Ladda ner en webbsida"
html_title:           "Elixir: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Varför

För att hämta en webbsida kan vara nödvändigt för att samla in data, såsom information om produkter eller användarfeedback. Det kan också vara ett sätt att automatisera en uppgift som skulle vara tidskrävande att göra manuellt.

##Så här gör du
```Elixir
url = "https://www.example.com"
{:ok, response} = HTTPoison.get(url)

response.status #returns 200
response.body #returns the HTML of the webpage
```

Först måste vi definiera URL: en som vi vill hämta. Sedan använder vi HTTPoison-paketet för att göra en GET-förfrågan till den URL:en, vilket ger oss tillbaka svaret i form av en tupel. Vi kan sedan komma åt status och kropp av svaret för att få informationen vi behöver.

```Elixir
url = "https://www.example.com"
headers = [{"User-Agent", "Elixir"}, {"Accept", "text/html"}]
{:ok, response} = HTTPoison.get(url, headers)
```

Om vi vill ställa in specifika headrar i vår förfrågan, som att ändra användaragenten eller acceptera en viss typ av innehåll, kan vi göra det genom att skicka med en lista över tuplar som innehåller namnet på headern och värdet som vi vill ställa in.

##Djupdykning

Att hämta en webbsida med Elixir är möjligt tack vare HTTPoison-paketet, som använder sig av Erlangs inbyggda httpc-modul för att göra förfrågningar. Det finns också andra paket som kan användas för denna uppgift, såsom Finch och Tesla. Dessa paket kan ge mer specifika funktioner och utökad funktionalitet, men för en grundläggande hämtning av en webbsida är HTTPoison en enkel och enkel lösning.

##Se även
- [HTTPoison dokumentation](https://hexdocs.pm/httpoison/api-reference.html)
- [Finch paket](https://hexdocs.pm/finch/readme.html)
- [Tesla paket](https://hexdocs.pm/tesla/readme.html)