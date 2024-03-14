---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:26.740663-07:00
description: "\xC5 parse HTML betyr \xE5 dissekere en nettsides oppmerking til noe\
  \ et program kan forst\xE5 og manipulere. Programutviklere parser HTML for \xE5\
  \ ekstrahere data,\u2026"
lastmod: '2024-03-13T22:44:40.748093-06:00'
model: gpt-4-0125-preview
summary: "\xC5 parse HTML betyr \xE5 dissekere en nettsides oppmerking til noe et\
  \ program kan forst\xE5 og manipulere. Programutviklere parser HTML for \xE5 ekstrahere\
  \ data,\u2026"
title: Analysering av HTML
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse HTML betyr å dissekere en nettsides oppmerking til noe et program kan forstå og manipulere. Programutviklere parser HTML for å ekstrahere data, automatisere webinteraksjoner eller migrere innhold mellom systemer.

## Hvordan:
Kotlin gjør det enkelt å parse HTML med biblioteker som Jsoup. Slik gjør du det:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Eksempelside</title></head><body><p>Dette er en test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val tittel = doc.title()
    println("Tittel: $tittel")  // Utdata: Tittel: Eksempelside

    val pTekst = doc.select("p").first()?.text()
    println("Avsnitt: $pTekst")  // Utdata: Avsnitt: Dette er en test.
}
```

Vi tar tak i tittel og avsnittstekst, bare for å skrape på overflaten av hva Jsoup kan gjøre. Men det er en start.

## Dypdykk:
Før Kotlin, var Java det vanlige valget for dette, ofte på en klønete måte. Jsoup endret spillet ved å tilby en jQuery-lignende tilnærming. Parsing av HTML er imidlertid ikke eksklusivt for Jsoup; andre biblioteker som HtmlUnit eller til og med regex (selv om det frarådes) eksisterer. Med Jsoup sikrer du at parsingen respekterer dokumentets struktur. Det bruker en DOM-modell, som muliggjør valg og manipulering av elementer. Det er motstandsdyktig også - det kan parse selv det rotete HTML.

## Se Også:
Dykk dypere inn i Jsoup:

- Jsoup offisiell dokumentasjon: https://jsoup.org/
- "Kotlin for Android Developers" bok: https://antonioleiva.com/kotlin-android-developers-book/
- Kotlin programmeringsspråk offisiell side: https://kotlinlang.org/

For bredere diskusjoner og veiledninger om web scraping og parsing:

- Web scraping med Kotlin og Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Parsing av HTML på Android med Kotlin og Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
