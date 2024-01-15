---
title:                "Att ladda ner en webbsida"
html_title:           "Fish Shell: Att ladda ner en webbsida"
simple_title:         "Att ladda ner en webbsida"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför
Att ladda ner en webbsida kan vara användbart av flera anledningar, som att spara en kopia för offline-visning eller att extrahera specifik information från sidan.

## Så här gör du
För att ladda ner en webbsida med Fish Shell behöver du först installera verktyget "curl". Detta kan göras med kommandot `brew install curl`. När det är klart, kan du använda följande kod för att ladda ner en sida:

```Fish Shell
curl -O <webbadress>
```

Detta sparar sidan som en fil på din dator. Om du vill ge filen ett annat namn kan du lägga till `-o <filnamn>` efter webbadressen.

## Djupdykning
"Curl" är ett kraftfullt verktyg som tillåter dig att göra mer än att bara ladda ner en sida. Du kan till exempel även använda det för att ladda ner flera sidor samtidigt, genom att lägga till flera webbadresser efter varandra i kommandot.

Om du vill extrahera specifik information från sidan kan du använda dig av "grep"-kommandot tillsammans med "curl". Till exempel, om du vill hitta alla länkar på en sida kan du använda följande kod:

```Fish Shell
curl <webbadress> | grep -o '<a[^>]* href="[^"]*"' 
```

Detta kommer att returnera en lista med alla länkar som finns på sidan.

## Se även
- [Fish Shell's officiella hemsida](https://fishshell.com)
- [Curl's officiella hemsida](https://curl.haxx.se)
- [En guide för grundläggande kommandon i Fish Shell](https://medium.com/@bekreev/the-ultimate-bash-vs-fish-shell-showdown-8258690b44e6)