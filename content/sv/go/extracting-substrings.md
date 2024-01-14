---
title:    "Go: Extrahera delsträngar"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt kan det vara nödvändigt att arbeta med textsträngar på ett mer avancerat sätt. En vanlig uppgift är att extrahera delsträngar från en större sträng baserat på ett visst mönster eller villkor. I detta blogginlägg kommer vi att utforska hur man kan åstadkomma detta i Go-programmeringsspråket.

## Hur man gör

För att extrahera en delsträng från en större sträng i Go, kan man använda funktionen "Substring". Denna funktion tar två parametrar, det första är den ursprungliga strängen och det andra är en gräns som definierar vilken del av strängen som ska extraheras. Här är ett exempel på hur man kan använda funktionen "Substring" i Go:

```Go
sträng := "Jag älskar att lära mig Go-programmering!"
delsträng := sträng[9:12]
fmt.Println(delsträng)
```

Detta kommer att producera följande utdata: "att".

För att extrahera en delsträng baserat på ett visst mönster eller villkor, kan man använda "Regular Expression" (regex) -tekniken i Go. Det finns olika paket och funktioner för att underlätta användningen av regex i Go, till exempel "regexp" -paketet och "FindStringSubmatch" -funktionen. Här är ett exempel på en regex-baserad delsträngsextraktion i Go:

```Go
sträng := "Min favoritfärg är blå"
regEx := regexp.MustCompile("blå")
delsträng := regEx.FindStringSubmatch(sträng)
fmt.Println(delsträng)
```

Detta kommer att producera följande utdata: ["blå"].

## Djupdykning

För mer komplicerade användningsfall kan det vara användbart att extrahera flera delsträngar från en större sträng samtidigt. Detta kan uppnås genom att använda "Split" -funktionen i Go, som delar upp en sträng baserat på ett visst separatorvärde. Här är en exempelkod på hur man kan använda "Split" -funktionen för att extrahera flera delsträngar från en sträng i Go:

```Go
sträng := "hund,katt,kanin,hamster"
delsträngar := strings.Split(sträng, ",")
fmt.Println(delsträngar)
```

Detta kommer att producera följande utdata: ["hund", "katt", "kanin", "hamster"].

## Se även

- Officiell Go-dokumentation för "Substring" -funktionen: https://golang.org/pkg/strings/#Substring
- Tutorial för Regular Expression (regex) i Go: https://golang.org/pkg/regexp/
- "Split" -funktionen i Go-dokumentationen: https://golang.org/pkg/strings/#Split