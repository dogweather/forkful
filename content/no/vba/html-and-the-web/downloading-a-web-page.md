---
aliases:
- /no/vba/downloading-a-web-page/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:32.546651-07:00
description: "\xC5 laste ned en nettside i Visual Basic for Applications (VBA) inneb\xE6\
  rer \xE5 hente HTML-innholdet p\xE5 en nettside fra Internett. Programmerere utf\xF8\
  rer ofte\u2026"
lastmod: 2024-02-18 23:08:53.726724
model: gpt-4-0125-preview
summary: "\xC5 laste ned en nettside i Visual Basic for Applications (VBA) inneb\xE6\
  rer \xE5 hente HTML-innholdet p\xE5 en nettside fra Internett. Programmerere utf\xF8\
  rer ofte\u2026"
title: Nedlasting av en nettside
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å laste ned en nettside i Visual Basic for Applications (VBA) innebærer å hente HTML-innholdet på en nettside fra Internett. Programmerere utfører ofte denne oppgaven for å behandle eller analysere innholdet på nettsteder programmert, fra inne i Excel, Access eller andre Office-applikasjoner.

## Hvordan:

For å laste ned en nettside i VBA, kan du bruke Microsoft XML, v6.0 (MSXML6)-biblioteket, som muliggjør server HTTP-forespørsler. Før du dykker inn i koden, sørg for at du har aktivert denne referansen i din VBA-redigerer ved å gå til `Verktøy` -> `Referanser` og merke av for `Microsoft XML, v6.0`.

Her er et enkelt eksempel på hvordan du kan laste ned HTML-innholdet på en nettside:

```basic
Sub LastNedNettside()
    Dim forespørsel As Object
    Dim url As String
    Dim respons As String
    
    ' Initialiser XML HTTP forespørselsobjektet
    Set forespørsel = CreateObject("MSXML2.XMLHTTP")
    
    url = "http://www.example.com"
    
    ' Åpne en synkron forespørsel
    forespørsel.Open "GET", url, False
    
    ' Send forespørselen til serveren
    forespørsel.send
    
    ' Hent responsteksten
    respons = forespørsel.responseText
    
    ' Skriv ut responsen i umiddelbart vindu (for feilsøkingsformål)
    Debug.Print respons
    
    ' Rydd opp
    Set forespørsel = Nothing
End Sub
```

Å kjøre denne subrutinen vil skrive ut HTML-en til `http://www.example.com` til det umiddelbare vinduet i VBA-redigereren. Merk at `False`-parameteren i `Open`-metoden gjør forespørselen synkron, noe som betyr at koden vil vente til nettsiden er lastet ned før den går videre til neste linje.

## Dypdykk

Teknikken som vises, er avhengig av MSXML, Microsofts implementering av XML HTTP Request-standarden, ofte brukt for AJAX-forespørsler i webutvikling. Denne komponenten har vært en del av Microsofts teknologistabel i lang tid, og gjør det til et robust valg for nettverksforespørsler i VBA.

Men, avhengigheten av MSXML og VBA for nedlasting og parsing av webinnhold kan være begrensende, spesielt med moderne webapplikasjoner som tungt bruker JavaScript for dynamisk innholdsgjengivelse. Disse begrensningene kan gjøre andre språk eller verktøy som Python med biblioteker som BeautifulSoup eller Selenium mer egnet for web-skrapingoppgaver på grunn av deres evne til å utføre JavaScript og håndtere komplekse nettstedinteraksjoner.

Til tross for dette, for enkle oppgaver som innebærer å hente enkel HTML-innhold eller når man arbeider innenfor Office-applikasjoner, forblir VBA et praktisk verktøy. Integrasjonen innen Office-pakken muliggjør direkte manipulasjon av dokumenter basert på webinnhold, og tilbyr en unik fordel for spesifikke brukstilfeller.
