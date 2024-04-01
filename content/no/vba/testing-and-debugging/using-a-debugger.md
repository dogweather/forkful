---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:36.744808-07:00
description: "\xC5 bruke en feils\xF8ker i Visual Basic for Applications (VBA) inneb\xE6\
  rer \xE5 kj\xF8re koden din steg for steg for \xE5 inspisere utf\xF8relsesflyten\
  \ og\u2026"
lastmod: '2024-03-13T22:44:40.625382-06:00'
model: gpt-4-0125-preview
summary: "\xC5 bruke en feils\xF8ker i Visual Basic for Applications (VBA) inneb\xE6\
  rer \xE5 kj\xF8re koden din steg for steg for \xE5 inspisere utf\xF8relsesflyten\
  \ og\u2026"
title: Bruker en debugger
---

## Hvordan:
I VBA er feilsøkeren en integrert del av Visual Basic Editor (VBE). Slik kan du utnytte den:

1. **Å sette brytepunkter**: Klikk i venstre marg ved siden av kodelinjen du er interessert i, eller plasser markøren på linjen og trykk F9. Dette forteller VBA å pause utførelsen når den når dette punktet.

    ```vb
    Sub DebugExample()
        Dim counter As Integer
        For counter = 1 Til 5
            Debug.Print counter ' Sett brytepunkt her
        Next counter
    End Sub
    ```

    Når koden utføres, vil den pause ved `Debug.Print counter` linjen, og lar deg inspisere variabelverdier.

2. **Steg Inn (F8)**: Med denne kommandoen utfører du kode en uttalelse om gangen, og går inn i eventuelle kalte prosedyrer. Det er nyttig for å spore hvordan koden din og funksjoner samhandler.

3. **Se-vinduet**: Bruk Se-vinduet til å overvåke verdier av variabler eller uttrykk. Hvis en variabel ikke er innenfor rekkevidde, indikerer Se-vinduet dette. Høyreklikk en variabel > Legg til overvåking.

4. **Umiddelbart vindu (Ctrl+G)**: Dette vinduet er spesielt nyttig for å teste uttrykk eller endre variabelverdier mens du feilsøker. Skriv `?variableName` for å skrive ut den nåværende verdien av en variabel, eller tildel en ny verdi med `variableName = newValue`.

    ```vb
    ' I umiddelbart vindu
    ?counter ' Skriver ut gjeldende verdi av counter
    counter = 3 ' Setter verdien av counter til 3
    ```

5. **Eksempel på utdata**:

    Når du når brytepunktet og utfører linje for linje ved hjelp av F8, kan det umiddelbare vinduet vise noe som dette:

    ```
    counter = 1
    counter = 2
    counter = 3
    ```

    Her har vi manuelt spurt om `counter`-variabelen etter hver iterasjon.

## Dypdykk:
Feilsøkeren i VBA, selv om den er robust, er del av en større tradisjon med feilsøkingsverktøy i programmeringsspråk, og har utviklet seg betydelig fra sine tidlige forgjengere. Introdusert med de første versjonene av VBA, hadde den som mål å gi utviklere et enkelt, men likevel kraftig sett med verktøy for kodeinspeksjon og rettelse. Over tid har forbedringer inkludert betingede brytepunkter, forbedrede overvåkningsfunksjoner og integrasjon med Excel-grensesnittet for mer intuitiv datainspeksjon.

Men sammenlignet med moderne integrerte utviklingsmiljøer (IDEer) som Visual Studio eller Eclipse, kan VBA sine feilsøkingsverktøy virke grunnleggende. Disse moderne IDEene tilbyr mer sofistikerte funksjoner som sanntidsvariabelinspeksjon, avanserte brytepunkter og integrerte enhetstestrammer. Mens disse alternativene gir mer omfattende feilsøkingsopplevelser, forblir enkelheten og direktheten til VBA sin feilsøker godt egnet til den spesifikke konteksten av automatisering og skripting innenfor Microsoft Office-applikasjoner.

For programmerere vant til disse moderne miljøene, kan justeringen til VBA sine feilsøkingsverktøy kreve en endring i tilnærming. Likevel, de grunnleggende prinsippene for å inspisere variabler, steg-for-steg gjennomgang av koden, og observasjon av kjøretidsatferd er universelle. Med praksis blir VBA sin feilsøker et uunnværlig verktøy for å sikre at automatiseringsskriptene dine fungerer feilfritt innenfor Office-økosystemet.
