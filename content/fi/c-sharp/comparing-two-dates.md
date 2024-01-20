---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vertailu kahden päivämäärän välillä C#-kielellä

## Mikä & Miksi?

Päivämäärien vertailulla tarkoitetalemme tilannetta, jossa tarkastelemme kahta eri päivämäärää ja määritämme niiden välisen ajanjakson. Ohjelmoijat tekevät tämän tiedon analysoimiseksi, aikataulutuksen helpottamiseksi tai ajanjaksojen määrittelemiseksi esimerkiksi varauksissa ja istunnoissa.

## Kuinka tehdä:

Alla on esimerkki siitä, kuinka vertailla kahta päivämäärää C#-kielellä.

```C#
DateTime date1 = new DateTime(2021, 6, 15);
DateTime date2 = new DateTime(2022, 6, 15);
int result = DateTime.Compare(date1, date2);

if (result < 0)
   Console.WriteLine("date1 is earlier than date2.");
else if (result == 0)
   Console.WriteLine("date1 is the same as date2.");
else
   Console.WriteLine("date1 is later than date2.");
```
Esimerkin tuloste:

```
date1 is earlier than date2.
```

## Syvällisempi katsaus:

**Historiallinen tausta**: Aikaisempi C#-versio käytti TimeSpan-objektia päivämäärien välisten erotusten määrittelemiseksi. Myöhemmissä versioissa käytämme DateTime.Compare-menetelmää, joka on enemmän intuitiivinen ja joustava.

**Vaihtoehdot**: DateTime.Compare ei ole ainoa tapa, jolla voimme verrata kahta päivämäärää C#-ohjelmointikielessä. Voimme myös vähentää kaksi päivämäärää toisistaan luomaan TimeSpan-objektin ja sitten analysoida sitä.

```C#
DateTime date1 = new DateTime(2021, 6, 15);
DateTime date2 = new DateTime(2022, 6, 15); 
TimeSpan span = date2 - date1;   
Console.WriteLine("There are {0} days between date1 and date2.", span.TotalDays);
```

**Implementaation yksityiskohdat**: DateTime.Compare vie parametrikseen kaksi DateTime-objektia ja palauttaa kokonaisluvun. Tämä kokonaisluku osoittaa ovatko päivämäärät samat, onko ensimmäinen päivämäärä aikaisempi tai myöhempi kuin toinen.

## Näe myös:

- DateTime luokka: [https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime?view=net-5.0](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime?view=net-5.0)
  
  
- Lisätietoja TimeSpan luokasta: [https://docs.microsoft.com/fi-fi/dotnet/api/system.timespan?view=net-5.0](https://docs.microsoft.com/fi-fi/dotnet/api/system.timespan?view=net-5.0)