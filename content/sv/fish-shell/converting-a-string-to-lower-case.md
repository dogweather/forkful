---
title:    "Fish Shell: Omvandla en sträng till små bokstäver"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Varför
Att omvandla en sträng till små bokstäver är en viktig och vanlig uppgift inom programmering. Det kan bidra till mer enhetliga och konsistenta resultat och underlätta jämförelse av olika strängar.

##Så här gör du
Det finns flera sätt att konvertera en sträng till små bokstäver i Fish Shell. Ett enkelt sätt är att använda kommandot `string tolower` tillsammans med strängen du vill konvertera inom ett `echo`-kommando. Till exempel:

```
Fish Shell
echo (string tolower "FISH Shell")
```

Detta kommer att producera följande utmatning:

```
fish shell
```

Som du kan se så har "FISH Shell" konverterats till små bokstäver.

Fish Shell erbjuder också möjligheten att använda kommandot `string lower` för att konvertera strängar. Detta kommando tar två parametrar: en sträng och ett index för den första positionen som ska konverteras. Till exempel:

```
Fish Shell
echo (string lower "Fish Shell" 1)
```

Detta kommer att ge samma utmatning som det första exemplet.

##Djupdykning
När du konverterar en sträng till små bokstäver i Fish Shell så används det Unicode-teckenuppsättningen för alla tecken. Detta betyder att bokstäver från alla språk kan konverteras korrekt till små bokstäver.

Om du behöver konvertera en sträng till små bokstäver i ett specifikt språk så kan du använda kommandot `set -l LC_CTYPE` för att ändra teckenuppsättningen. Till exempel, om du vill konvertera en sträng till små bokstäver i svenska, så kan du använda följande kommando:

```
echo (set -lx LC_CTYPE 'sv_SE.UTF-8'; string tolower "Äpple")
```

Detta kommer att producera utmatningen "äpple".

##Se också
- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [Officiell Fish Shell GitHub-sida](https://github.com/fish-shell/fish-shell)
- [Fish Shell forum](https://github.com/fish-shell/fish-shell)