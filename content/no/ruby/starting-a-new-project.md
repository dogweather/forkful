---
title:    "Ruby: Å begynne på et nytt prosjekt"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

#Hvorfor

Å starte et nytt programmeringsprosjekt kan være en spennende og givende opplevelse. Det gir deg muligheten til å utforske nye ideer, lære nye teknologier og kanskje til og med bidra til å løse et problem du brenner for. Det er også en flott måte å utfordre deg selv og forbedre dine programmeringsferdigheter.

#Slik gjør du det

For å starte et nytt prosjekt i Ruby, må du først ha Ruby installert på datamaskinen din. Deretter kan du følge disse enkle trinnene:

1. Åpne din favoritt tekstredigerer og lagre en ny fil med ".rb" filtype. Dette vil være filen der koden din vil bli skrevet.

2. Start ved å definere et nytt objekt ved hjelp av `class` kommandoen. Dette vil være grunnlaget for ditt prosjekt.

3. Deretter kan du begynne å legge til funksjoner og metoder til objektet ditt ved hjelp av `def` kommandoen.

4. Du kan også legge til variabler og andre datastrukturer for å håndtere data i prosjektet ditt.

5. Når du er ferdig med å skrive koden din, kan du kjøre den ved å åpne terminalen og skrive `ruby filename.rb` kommandoen. Dette vil utføre koden din og vise resultatet til deg.

```Ruby
class MittProsjekt
  def initialize(navn)
    @navn = navn
  end

  def si_hei
    puts "Hei, mitt navn er #{@navn}!"
  end
end

min_objekt = MittProsjekt.new("Maren")
min_objekt.si_hei
```

Dette vil gi følgende utdata i terminalen:

`Hei, mitt navn er Maren!`

#Dykk dypere

Etter å ha laget et grunnleggende Ruby-prosjekt, kan du begynne å utforske dype områder av programmeringsspråket. Du kan lese dokumentasjon, lære om ulike biblioteker og eksperimentere med mer avanserte konsepter som objektorientert programmering, feilhåndtering og parallellprogrammering.

En annen måte å dykke dypere inn i Ruby på, er å delta på meetups, konferanser eller kurs. Dette vil gi deg muligheten til å møte andre Ruby-entusiaster og lære av deres erfaringer.

#Se også

- [Offisiell Ruby-dokumentasjon](https://www.ruby-lang.org/en/documentation/)
- [Ruby Meetups i Norge](https://www.meetup.com/topics/ruby/no/)
- [RubyConf Norge](https://rubyconf.no/)