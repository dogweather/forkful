---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:19.709788-07:00
description: "Stel je voor dat je er net achter komt dat je een batch-update moet\
  \ uitvoeren op verschillende configuratiebestanden op je server. Je zou elk bestand\u2026"
lastmod: '2024-03-13T22:44:50.978094-06:00'
model: gpt-4-0125-preview
summary: "Stel je voor dat je er net achter komt dat je een batch-update moet uitvoeren\
  \ op verschillende configuratiebestanden op je server. Je zou elk bestand\u2026"
title: Bestanden ter plekke bewerken met CLI one-liners
---

{{< edit_this_page >}}

## Wat & Waarom?

Stel je voor dat je er net achter komt dat je een batch-update moet uitvoeren op verschillende configuratiebestanden op je server. Je zou elk bestand kunnen openen, de wijzigingen handmatig aanbrengen en ze opslaan. Of, je kunt ter plekke bewerken direct vanuit je command-line interface (CLI), een vaardigheid die tijd bespaart, fouten vermindert en repetitieve taken automatiseert. Deze techniek is vooral handig voor systemische updates, correcties of bulkwijzigingen waar handmatige aanpassingen onpraktisch of foutgevoelig zouden zijn.

## Hoe te:

Als het gaat om het ter plekke bewerken van bestanden met behulp van Bash, komen twee prominente hulpmiddelen in het spel: `sed` en `awk`. Laten we bekijken hoe we deze krachtige hulpmiddelen kunnen gebruiken met enkele codeervoorbeelden.

### `sed` gebruiken voor eenvoudige tekstvervanging

Het volgende commando vervangt de eerste voorkomst van "text1" door "text2" in `file.txt`:

```Bash
sed -i 's/text1/text2/' file.txt
```

Voor een globale vervanging (alle voorkomens), zou je een `g` aan het einde toevoegen:

```Bash
sed -i 's/text1/text2/g' file.txt
```

Om meerdere bestanden tegelijk te wijzigen:

```Bash
sed -i 's/text1/text2/g' file1.txt file2.txt file3.txt
```

### `awk` gebruiken voor complexere manipulaties

`awk` is een ander hulpmiddel dat uitblinkt met zijn programmeermogelijkheden, vooral nuttig voor tekstverwerking die veldgebaseerde gegevens omvat.

Het tweede veld van elke regel veranderen in `newValue` in `data.csv`, gescheiden door komma's:

```Bash
awk -i inplace -F, '{$2="newValue"; print $0}' OFS=, data.csv
```

### Backup voordat je springt

Een praktisch advies: maak altijd een backup voordat je ter plekke gaat bewerken. `sed` vergemakkelijkt dit met de `-i` optie gevolgd door een suffix om een backup te maken.

```Bash
sed -i.bak 's/text1/text2/g' file.txt
```

Dit commando maakt een backup van het originele `file.txt` als `file.txt.bak` voordat de vervanging wordt uitgevoerd.

## Dieper Duiken

Het vermogen om bestanden direct vanaf de command line te bewerken is ontstaan als een natuurlijke voortgang van de filosofie van Unix: gebruikers in staat stellen om efficiënt te beheren en data te manipuleren met zo min mogelijk toetsaanslagen. Toch heeft deze kracht zijn voorbehouden.

### Historische context

Unix-hulpmiddelen zoals `sed` en `awk` bestaan al sinds de vroege dagen van Unix, gemaakt als onderdeel van de toolkitfilosofie, gericht op gespecialiseerde, samenstelbare commando's. Hun opname in het arsenaal van Unix was een reactie op de behoefte aan efficiënte tekstverwerking in een landschap gedomineerd door command-line interfaces.

### Alternatieven

Hoewel `sed` en `awk` krachtig zijn, zijn ze niet de enige opties. Perl en Python hebben bijvoorbeeld command-line opties (`-p` en `-i`, respectievelijk) die vergelijkbare ter plekke bewerkingsmogelijkheden toestaan met mogelijk meer leesbare syntaxis voor complexe operaties.

```Bash
perl -pi -e 's/text1/text2/g' file.txt
```

```Bash
python -c "import fileinput, sys; [sys.stdout.write(line.replace('text1', 'text2')) for line in fileinput.input(files='file.txt', inplace=True)]"
```

Elk alternatief heeft zijn sterke punten: Perl's mogelijkheden voor one-liners zijn immens, en Python's syntaxis is mogelijk toegankelijker voor degenen die niet diep ingewijd zijn in de Unix tekstverwerkingstools.

### Implementatiedetails

Ter plekke bewerken is technisch gezien niet echt "ter plekke". Zowel `sed -i` als `awk -i inplace` werken door een tijdelijk bestand te creëren waarin de verwerkte uitvoer wordt opgeslagen voordat het originele bestand wordt vervangen. Deze aanpak zorgt ervoor dat het bestand niet beschadigd raakt mocht het proces worden onderbroken. De implicaties liggen voornamelijk op het gebied van middelen en machtigingen: je moet genoeg schijfruimte hebben voor het tijdelijke bestand en de machtigingen om bestanden te kunnen creëren in de map van je doelbestand.

Hoewel krachtig, moeten in-place bewerkingsopdrachten met voorzichtigheid worden gebruikt. Een verkeerd geplaatste regex kan leiden tot gegevensverlies, wat het belang van backups benadrukt. Ondanks potentiële valkuilen, kunnen deze commando's aanzienlijk je mogelijkheid verbeteren om snel, efficiënt bestandswijzigingen rechtstreeks vanaf de command line uit te voeren, belichamend de Unix-filosofie van het benutten van eenvoudige, krachtige hulpmiddelen om complexe taken te volbrengen.
