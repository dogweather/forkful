---
title:                "Att få aktuellt datum"
html_title:           "Ruby: Att få aktuellt datum"
simple_title:         "Att få aktuellt datum"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Du kanske undrar varför det är viktigt att få den nuvarande datumet i Ruby? Det finns flera anledningar till detta. Först och främst behöver många program och applikationer det aktuella datumet för att fungera korrekt och för att kunna hålla reda på tiden. Dessutom kan det vara användbart för att skapa rapporter, scheman eller andra typer av dokument som kräver aktuell tidsinformation.

## Hur man gör det
För att få det aktuella datumet i Ruby behöver du använda dig av en inbyggd metod som heter `Time.now`. Här är ett exempel på hur du kan använda denna metod för att få datumet:

```Ruby
nuvarande_dag = Time.now  # lägger till den nuvarande tiden i variabeln nuvarande_dag
puts nuvarande_dag        # skriver ut den nuvarande tiden
```

Detta kommer att skriva ut datumet och tiden i formatet `YYYY-MM-DD HH:MM:SS +/-TTTT` där YYYY är året, MM är månaden, DD är dagen, HH är timmen, MM är minuterna, SS är sekunderna och TTTT är tidszonen.

Om du bara är intresserad av att få en specifik del av datumet, till exempel bara månad eller år, kan du använda dig av olika metoder som `month` eller `year`. Här är ett exempel på hur du kan göra det:

```Ruby
nuvarande_dag = Time.now        # lägger till den nuvarande tiden i variabeln nuvarande_dag
månad = nuvarande_dag.month     # hämtar månaden från tidens objekt
år = nuvarande_dag.year         # hämtar året från tidens objekt
puts "Det är just nu #{månad}-#{år}"
```

Detta kommer att skriva ut en sträng som säger något i stil med "Det är just nu 9-2021" beroende på vilken månad och år det är när du kör koden.

## Djupdykning
Om du vill ha mer kontroll över hur datumet visas kan du använda dig av den inbyggda metoden `strftime`. Denna metod låter dig formatera datumet enligt dina egna preferenser. Här är ett exempel på hur du kan använda den:

```Ruby
nuvarande_dag = Time.now         # lägger till den nuvarande tiden i variabeln nuvarande_dag
formaterat_datum = nuvarande_dag.strftime("%d/%m/%Y")    # formaterar datumet till DD/MM/YYYY
puts "Dagens datum är #{formaterat_datum}."
```

Detta kommer att skriva ut en sträng som säger något i stil med "Dagens datum är 21/09/2021." Om du vill ha mer information om alla olika formatalternativ som `strftime` erbjuder kan du läsa om det i Ruby's dokumentation.

## Se även
- [Ruby's dokumentation för Time-klassen](https://ruby-doc.org/core-3.0.2/Time.html)
- [En artikel om hur man hanterar tid i Ruby](https://www.rubyguides.com/2015/10/ruby-datetime/)
- [En stack overflow fråga om användande av `strftime`](https://stackoverflow.com/questions/9012270/ruby-strftime-commas-and-am-pm)