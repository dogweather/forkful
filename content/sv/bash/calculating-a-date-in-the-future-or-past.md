---
title:                "Bash: Räkna ut ett datum i framtiden eller förflutna"
simple_title:         "Räkna ut ett datum i framtiden eller förflutna"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna kan vara mycket användbart i en rad olika situationer. Till exempel kanske du behöver planera en resa och vill veta vilken dag det är om två veckor, eller så kanske du måste beräkna när en elektiv operation ska äga rum baserat på patientens ålder. Det finns otaliga scenarier där denna färdighet kan komma till nytta.

## Så här gör du

För att kunna beräkna ett datum i framtiden eller det förflutna behöver du använda dig av Bash-programmering. Det låter kanske avancerat, men det är faktiskt ganska enkelt. Här är ett exempel på hur du skulle gå till väga för att beräkna datumet för nästa onsdag:

```Bash
date -d “next Wednesday”
```

Efter att du har skrivit in detta i terminalen och tryckt på enter, kommer du få en utskrift med det eftertraktade datumet. Byt ut “next Wednesday” mot vilken dag som helst som du vill beräkna. Du kan också lägga till eller dra bort antalet dagar från det nuvarande datumet genom att använda en plus eller minus mellanrumsavgränsare. Till exempel:

```Bash
date -d “+2 weeks”
```

Detta kommer att ge dig datumet om två veckor från idag. Det finns också andra användbara flaggor och kommandon som du kan använda för att precisionera din datumberäkning. Se till att läsa dokumentationen för att lära dig mer.

## Djupdykning

För de som är intresserade av att lära sig mer om hur man beräknar datum i Bash-programmering, finns det en hel del att utforska. När man behärskar detta koncept kan man börja kombinera det med andra tekniker och skapa avancerade program för att hantera datum och tider. Det är också en nyttig färdighet att ha om man arbetar med skriptning eller automatisering av uppgifter.

## Se även

- Bash Programlama 101: https://www.sk.com.tr/bash-programlama-101/
- The Linux Command Line: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/
- Linux Bash: https://www.gnu.org/software/bash/