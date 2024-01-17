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

Webbsunder is a powerful tool in Elixir for downloading web pages. Programmers often use this function to extract data from websites or to automate certain tasks related to web browsing.

### Vad & Varför?
Ladda ner en webbsida är helt enkelt en kod som tillåter dig att hämta innehållet på en webbsida. Detta är särskilt användbart när du behöver få tag på data från en specifik webbplats eller när du vill automatisera vissa uppgifter som är relaterade till webbläsning.

### Så här gör du:
Att ladda ner en webbsida i Elixir är enkelt med hjälp av det inbyggda biblioteket HTTPoison. Här är ett exempel på hur du kan använda funktionen Webbsunder för att ladda ner en webbsida och spara den i en fil:

```Elixir
url = "https://www.example.com/"
{:ok, response} = HTTPoison.get(url)
File.write("example.html", response.body)
```

Detta kodblock gör en GET-förfrågan till den angivna webbplatsen och sparar svaret i en fil med namnet "exempel.html". Du kan ändra namnet på filen och destinationen enligt dina behov.

### Djupdykning:
Webbsunder är en del av det inbyggda HTTPoison-biblioteket och används för att begära webbsidor. Det finns också andra alternativ för att hämta webbsidor i Elixir, såsom Tesla och Mint. Du kan också använda HTTPoison för att utföra andra HTTP-begäranden som till exempel POST, PUT och DELETE.

När du laddar ner en webbsida med Webbsunder returneras en tupel som innehåller svaret från webbservern och eventuella fel eller statuskod. Du kan utforska detta svar ytterligare för att extrahera specifik information du behöver från webbsidan.

### Se också:
Du kan läsa mer om Webbsunder och HTTPoison i deras officiella dokumentation på https://hexdocs.pm/httpoison/HTTPoison.html och https://hexdocs.pm/webbsunder/Webbsunder.html. Du kan också utforska alternativen Tesla och Mint för att hämta webbsidor i Elixir.