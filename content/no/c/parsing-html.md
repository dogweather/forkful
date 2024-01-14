---
title:                "C: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/parsing-html.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du noen gang har besøkt en nettside og lurt på hvordan den faktisk fungerer, har du kanskje også lurt på hva HTML-koden som utgjør siden faktisk betyr. Kanskje du til og med har ønsket å hente informasjon fra en nettside og bruke den til noe annet, som å lage en app eller automatisk fylle ut skjemaer. Hvis dette høres ut som deg, er det å lære hvordan man parser HTML en verdifull ferdighet å ha.

# Hvordan gjøre det

Først må vi forstå hva som egentlig menes med å "parse" HTML. Å parse betyr å analysere en streng av tekst og konvertere den til et mer strukturert format. I vårt tilfelle vil vi konvertere HTML-koden til et format som en datamaskin kan forstå og manipulere.

La oss si at vi ønsker å hente tittelen på en nettside. Her er et eksempel på HTML-koden vi kan forvente å finne:

```
<html>
<head>
<title>Dette er tittelen på nettsiden</title>
</head>
<body>
<h1>Velkommen til nettsiden min</h1>
</body>
</html>

```

Hvis vi ønsker å hente tittelen, kan vi bruke noen få linjer med C-kode for å trekke ut teksten som er inneholdt i <title> -taggen. Det ville se noe slik ut:

```
char* html_string = "<html><head><title>Dette er tittelen på nettsiden</title></head><body><h1>Velkommen til nettsiden min</h1></body></html>";
char* title_start = strstr(html_string, "<title>") + strlen("<title>");
char* title_end = strstr(html_string, "</title>");
char title[100];
memcpy(title, title_start, title_end-title_start);
printf("%s", title);

```

Dette vil gi oss følgende utdata:

```
Dette er tittelen på nettsiden

```

# Dypdykk

Nå som vi har en grunnleggende forståelse av hvordan man kan hente data fra HTML-kode, kan vi ta et dypere dykk inn i temaet. HTML-kode er bygget opp av ulike tags og attributter som forteller nettleseren hvordan innholdet skal vises. Det er viktig å ha en god forståelse av disse tags og attributter for å kunne hente den informasjonen du trenger.

I tillegg kan det være lurt å se på ulike biblioteker og verktøy som kan hjelpe deg med å parse HTML-kode på en mer effektiv måte. Et populært verktøy er for eksempel "libxml" som tilbyr funksjoner for å lette parsing av HTML-kode.

# Se også

- [Libxml](http://www.xmlsoft.org/index.html)
- [HTML Dom](https://www.w3schools.com/js/js_htmldom.asp)
- [Parsing HTML using c](https://stackoverflow.com/questions/538395/parsing-html-using-c)