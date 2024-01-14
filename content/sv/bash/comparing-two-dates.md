---
title:    "Bash: Jämföring av två datum"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är ett vanligt problem inom programmering, särskilt när man arbetar med tidsrelaterade data eller behöver filtrera och sortera dem baserat på datum. I denna bloggpost kommer vi att titta närmare på hur du enkelt kan jämföra två datum i Bash, med hjälp av några praktiska kodexempel.

## Hur man gör
För att jämföra två datum i Bash, behöver vi först följa ett enkelt format som följer den ISO-standard som brukar användas för representation av datum: YYYY-MM-DD. Vi kan också behöva konvertera våra datum till Unix-timestamps, vilket är en numerisk representation av datumet i sekunder sedan 1 januari 1970. För detta kan vi använda `date`-kommandot i Bash, tillsammans med lämpliga formateringsflaggor.

Nedan följer ett enkelt exempel på hur man kan jämföra två datum baserat på om de är lika, större eller mindre än varandra:

```Bash
# Skapa två variabler som innehåller våra datum
datum1="2021-08-15"
datum2="2021-08-20"

# Konvertera datum till Unix-timestamps
timestamp1=$(date -d "$datum1" +"%s")
timestamp2=$(date -d "$datum2" +"%s")

# Jämför med hjälp av if-satser och enkel konsol-utmatning
if [ $timestamp1 -eq $timestamp2 ] ; then
  echo "Dagen $datum1 är lika med dagen $datum2"
elif [ $timestamp1 -gt $timestamp2 ] ; then
  echo "Dagen $datum1 är större än dagen $datum2"
else
  echo "Dagen $datum1 är mindre än dagen $datum2"
fi
```

Körningsresultat:

```Bash
Dagen 2021-08-15 är mindre än dagen 2021-08-20
```

Vi kan också jämföra datum baserat på tidsintervaller, som till exempel om ett datum ligger mellan två andra datum. I följande exempel ser vi hur man kan använda en variabel för att ange ett intervall och sedan jämföra det med givna datum:

```Bash
# Skapa en variabel som innehåller antalet dagar (10)
antal_dagar=10

# Konvertera datum till Unix-timestamps
datum1=$(date -d "2021-07-01" +"%s")
datum2=$(date -d "2021-07-20" +"%s")

# Jämför om ett tredje datum ligger inom givet tidsintervall
jamt=$(date -d "2021-07-$antal_dagar" +"%s")

# Använd en if-sats och enkel konsol-utmatning
if [ $jamt -ge $datum1 ] && [ $jamt -le $datum2 ] ; then
  echo "Dagen 2021-07-$antal_dagar ligger mellan 2021-07-01 och 2021-07-20"
else
  echo "Dagen 2021-07-$antal_dagar ligger inte mellan 2021-07-01 och 2021-07-20"
fi
```

Körningsresultat:

```Bash
Dagen 2021-07-10 ligger mellan 2021-07-01 och 2021-07-20
```

## Fördjupning
För att verkligen förstå hur man jämför datum i Bash, behöver vi också veta vad Unix-timestamps är och hur man konverterar datum till dessa. Unix-timestamps är ett numeriskt värde som representerar antalet sekunder som gått sedan 1 januari 1970, och det är en standard inom Unix-system. För att konvertera ett datum till en Unix-timestamp, behöver man använda `date`-kommandot tillsammans med formateringsflaggor som `%s`.

När vi jämför två datum i Bash, är det också viktigt att komma ihåg att ta hänsyn