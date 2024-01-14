---
title:                "Ruby: Sända en http förfrågan"
simple_title:         "Sända en http förfrågan"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför HTTP-förfrågan är viktig i Ruby

Att kunna skicka HTTP-förfrågor med Ruby är en viktig kunskap i utvecklarens verktygslåda. Genom att kunna skicka förfrågor till andra servrar kan du till exempel hämta data från en API, ladda in externa filer, eller lagra information i en databas. Detta gör det möjligt att skapa dynamiska webbapplikationer och automatisera uppgifter.

## Hur man skickar en HTTP-förfrågan i Ruby

För att skicka en HTTP-förfrågan i Ruby behöver du först använda dig av ett bibliotek som stödjer HTTP-anrop, till exempel "net/http". Sedan kan du definiera vilken typ av förfrågan du vill skicka (GET, POST, PUT, DELETE) och vilken URL du vill skicka förfrågan till.

```Ruby
require 'net/http' #Importera net/http biblioteket
uri = URI('http://www.example.com') #Definiera URL
response = Net::HTTP.get(uri) #Skicka GET-förfrågan
puts response #Skriv ut svaret
```

Detta kodexempel skickar en GET-förfrågan till www.example.com och skriver ut svaret i terminalen.

## En djupdykning i HTTP-förfrågan

En HTTP-förfrågan består av flera delar, som headers, body och statuskoder. Headers innehåller metadata om förfrågan såsom vilken typ av förfrågan det är, vilken typ av data som skickas, och vilken språkkod som används. Body är den data som skickas med förfrågan, till exempel en sträng eller ett objekt. Statuskoder används för att ange om förfrågan lyckades och i så fall vilken typ av svar som skickas tillbaka.

Det är också viktigt att vara medveten om säkerheten vid skickandet av HTTP-förfrågor. Användningen av HTTPS istället för HTTP innebär att förfrågan och svaret krypteras för att skydda känslig data.

## Se även

- [Net/HTTP bibliotekets dokumentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [En guide till HTTP-förfrågan med Ruby](https://www.rubyguides.com/2018/08/ruby-http-request/)
- [En djupdykning i hur HTTP fungerar](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)