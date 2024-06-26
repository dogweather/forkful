---
date: 2024-01-27 16:20:32.875013-07:00
description: "Hur man g\xF6r: Ruby erbjuder ett enkelt s\xE4tt att redigera filer\
  \ p\xE5 plats direkt fr\xE5n kommandoraden. Genom att anv\xE4nda Ruby's `-i`-v\xE4\
  xel kan du be Ruby att\u2026"
lastmod: '2024-03-13T22:44:38.426341-06:00'
model: gpt-4-0125-preview
summary: "Ruby erbjuder ett enkelt s\xE4tt att redigera filer p\xE5 plats direkt fr\xE5\
  n kommandoraden."
title: "Redigera filer p\xE5 plats med kommandoradsenradare"
weight: 32
---

## Hur man gör:
Ruby erbjuder ett enkelt sätt att redigera filer på plats direkt från kommandoraden. Genom att använda Ruby's `-i`-växel kan du be Ruby att verka direkt på de angivna filerna. Låt oss leka med några exempel för att se hur detta fungerar i verkliga livet. Tänk dig att du har en fil `greetings.txt` med följande innehåll:

```
Hello, world!
Hello, Ruby!
Hello, programming!
```

Och du vill ersätta ordet "Hello" med "Hi". Så här kan du göra det:

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

Efter att ha kört det här kommandot kommer `greetings.txt` att uppdateras till:

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

Om du är orolig för att potentiellt ställa till med data, har Ruby täckt dig. Genom att ange en utvidgning till `-i`-växeln skapar Ruby en backup innan ändringarna verkställs. Till exempel:

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

Nu, tillsammans med din redigerade `greetings.txt`, kommer du att hitta en `greetings.txt.bak` i samma mapp, som håller det ursprungliga innehållet.

## Djupdykning
Magin med att redigera filer på plats i Ruby kommer från dess kombination av Perl-lik textbearbetningskapacitet och Rubys egen syntaktiska elegans. Historiskt sett var Perl det givna språket för snabba enradsscript, särskilt för textmanipulation. Ruby anammade detta paradigm, vilket tillåter kraftfulla kommandoradsscriptingmöjligheter.

Alternativ för att redigera på plats finns i andra språk, såsom Perl självt och sed, en strömredigerare i Unix-system. Var och en har sina styrkor – Perl är känt för sin textbearbetningskraft medan sed är oöverträffat i sin enkelhet för strömredigeringsuppgifter. Dock erbjuder Ruby en balans, som tillhandahåller robust textmanipulation med en mer läsbar och användarvänlig syntax, särskilt för de som redan är bekanta med Ruby.

På genomförandesidan fungerar Rubys redigering på plats genom att byta namn på den ursprungliga filen, skapa en ny med det ursprungliga filnamnet, och sedan skriva ändringarna till denna nya fil samtidigt som den läses från den omdöpta originalfilen. Detta tillvägagångssätt säkerställer operationens atomitet; antingen bearbetas hela filen framgångsrikt, eller så görs inga ändringar alls, vilket skyddar integriteten för dina data under redigeringsprocessen. Denna mekanism, kombinerat med Rubys undantagshantering, ger även motståndskraft mot avbrott, såsom strömavbrott eller processavslut, och säkerställer att åtminstone backupen förblir intakt.

Sammanfattningsvis är Rubys redigering av filer på plats ett bevis på dess användbarhet som ett scriptingspråk, som erbjuder en blandning av kraft, enkelhet och elegans för textmanipuleringsuppgifter direkt från kommandoraden.
