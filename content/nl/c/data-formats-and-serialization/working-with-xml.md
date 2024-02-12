---
title:                "Werken met XML"
date:                  2024-02-03T18:12:59.848917-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-xml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met XML in C houdt in dat je XML-documenten parseert, opvraagt en manipuleert met behulp van verschillende bibliotheken. Programmeurs werken met XML vanwege het veelvuldige gebruik in webservice, configuratiebestanden en gegevensuitwisseling tussen verschillende systemen, wat vaardigheden in het efficiënt omgaan met XML noodzakelijk maakt voor robuuste applicatieontwikkeling.

## Hoe te:

C heeft geen ingebouwde ondersteuning voor XML, dus je moet externe bibliotheken gebruiken. Een populaire keuze is `libxml2`, een stabiele en functie-rijke bibliotheek. Hier is hoe je een XML-bestand leest en parseert met `libxml2`.

Zorg eerst dat je `libxml2` op je systeem hebt geïnstalleerd. Mogelijk moet je dit installeren via je pakketbeheerder (bijv. `apt-get install libxml2-dev` op Debian-systemen).

Voeg vervolgens de `libxml2` header toe aan je C-programma:

```c
#include <libxml/parser.h>
#include <libxml/tree.h>
```

Nu, laten we een eenvoudig programma schrijven om een XML-bestand te parsen en de namen van de eerstelijns elementen uit te printen:

```c
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main(void) {
    xmlDoc *document = NULL;
    xmlNode *root_element = NULL;

    // Initialiseer de bibliotheek en controleer op potentiële ABI-mismatches
    LIBXML_TEST_VERSION

    // Parseer het bestand en krijg de DOM
    document = xmlReadFile("your_file.xml", NULL, 0);

    if (document == NULL) {
        printf("Het parseren van het XML-bestand is mislukt\n");
        return -1;
    }

    //Krijg het root element node
    root_element = xmlDocGetRootElement(document);

    for (xmlNode *currentNode = root_element; currentNode; currentNode = currentNode->next) {
        if (currentNode->type == XML_ELEMENT_NODE) {
            printf("Node Type: Element, naam: %s\n", currentNode->name);
        }
    }

    // Vrijgeven van het geheugen dat voor de parser en de DOM is gealloceerd
    xmlFreeDoc(document);

    // Opruimen en controleren op lekken
    xmlCleanupParser();
    xmlMemoryDump(); // Optioneel

    return 0;
}
```

Om dit programma te compileren, zorg ervoor dat je linkt tegen `libxml2`:

```sh
gcc -o xml_example xml_example.c $(xml2-config --cflags --libs)
```

Als je een XML-bestand genaamd `your_file.xml` hebt, zou het uitvoeren van het gecompileerde programma de namen van de eerstelijns elementen moeten afdrukken.

## Diepere Duik

De interactie tussen C en XML is een verhaal van het samenbrengen van twee zeer verschillende werelden: het gestructureerde, op bytes gebaseerde, procedurele paradigma van C en het hiërarchische, breedsprakige en op documenten gerichte model van XML. Bij het integreren van XML-verwerkingsmogelijkheden in C-programma's, benutten ontwikkelaars de sterke punten van C - zoals snelheid en laag niveau geheugen toegang - om XML-documenten efficiënt te parsen en te manipuleren.

`libxml2`, ontwikkeld als onderdeel van het GNOME-project, is uitgegroeid tot de de facto standaard voor XML-verwerking in C vanwege de uitgebreide ondersteuning voor XML-standaarden en de prestaties. Het belichaamt jaren van ontwikkelingsinspanningen en bijdragen van de gemeenschap, waardoor het robuust en efficiënt is voor de meeste XML-taken.

Hoewel `libxml2` krachtige mogelijkheden biedt, is het vermeldenswaard dat de complexiteit van XML-parsing en manipulatie aanzienlijke overhead kan introduceren. In scenario's waar de breedsprakigheid en complexiteit van XML ongerechtvaardigd zijn, kunnen alternatieven zoals JSON de voorkeur hebben voor gegevensuitwisseling. Desalniettemin, voor op XML-gecentreerde applicaties of omgevingen waar XML-gebruik diepgeworteld is, stelt het beheersen van `libxml2`-gebruik in C ontwikkelaars in staat om te werken met een breed scala aan XML-documenten en API's, en overbrugt het de kloof tussen de programmeertaal C en de wereld van gestructureerde documentverwerking.
