---
title:                "Arbeta med CSV"
html_title:           "Javascript: Arbeta med CSV"
simple_title:         "Arbeta med CSV"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

Att jobba med CSV-filer kan vara en användbar färdighet för dig som arbetar med datahantering, analys eller programmering. CSV står för "Comma-Separated Values" och det är ett enkelt filformat som låter dig lagra stora mängder strukturerad data på ett enkelt sätt.

## Hur man gör det

CSV-filer kan tas emot och hanteras på flera olika sätt i Javascript. Det enklaste sättet är att använda en befintlig modul eller bibliotek. Ett populärt alternativ är "Papa Parse", som tillåter dig att enkelt importera en CSV-fil och läsa av dess innehåll. Nedan följer ett enkelt exempel på hur du kan använda "Papa Parse" för att läsa av en CSV-fil och sedan skriva ut dess innehåll i konsolen:

```Javascript
var csvFile = "example.csv"; //ange namnet på din CSV-fil här

Papa.parse(csvFile, {
    download: true, //talar om för "Papa Parse" att ladda ner filen från filvägen
    complete: function(results) { //använder en callback-funktion för att få ut resultatet
        console.log(results.data); //skriver ut innehållet i filen i konsolen
    }
});
```

Om du vill skriva innehållet från CSV-filen i en HTML-tabell kan du göra det med hjälp av "Papa Parse" och vanlig Javascript kod. Nedan följer ett exempel på hur du kan göra det:

```Javascript
var table = document.getElementById("myTable"); //hämtar en befintlig tabell från HTML-koden
var csvFile = "example.csv"; //ange namnet på din CSV-fil här

Papa.parse(csvFile, {
    download: true, //talar om för "Papa Parse" att ladda ner filen från filvägen
    complete: function(results) { //använder en callback-funktion för att få ut resultatet
        results.data.forEach(function(row) { //loopar igenom varje rad
            var newRow = table.insertRow(); //lägger till en ny rad i tabellen
            row.forEach(function(cell) { //loopar igenom varje cell i raden
                var newCell = newRow.insertCell(); //lägger till en ny cell i raden
                newCell.innerHTML = cell; //lägger till cellens innehåll i HTML-koden
            });
        });
    }
});
```

Det är också möjligt att lägga till nya datapunkter i en befintlig CSV-fil med hjälp av "Papa Parse". Nedan följer ett exempel där vi lägger till en ny rad med data och sparar den nya versionen av filen:

```Javascript
var newData = ["Jane", "Doe", "jane@gmail.com"]; //skapar en array med ny data att lägga till
var csvFile = "example.csv"; //ange namnet på din CSV-fil här

Papa.parse(csvFile, {
    download: true, //talar om för "Papa Parse" att ladda ner filen från filvägen
    complete: function(results) { //använder en callback-funktion för att få ut resultatet
        results.data.push(newData); //lägger till den nya data-arrayen i filens data
        var csv = Papa.unparse(results.data); //konverterar den uppdaterade datan till CSV-format
        var link = document.createElement("a"); //skapar en länk för att spara filen
        link.download = "updated_file.csv"; //anger filnamnet för den uppdaterade filen
        link.href = URL.createObjectURL(new Blob([csv], { type: "text/csv" })); //skapar en Blob med filens innehåll och länkar hän till den ny skapade länken
        link.click(); //klickar på länken för att spara den uppdaterade filen
    }
});
```

## Djupdykning

För de som är intresserade av mer avancerad datahantering med CSV i Javascript finns det flera andra möjligheter och bibliotek att utforska. Ett exempel är "d3-dsv", ett bibliotek som konverterar CSV-data till