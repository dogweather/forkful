---
title:                "Arbeta med csv"
html_title:           "PHP: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har arbetat med data i en tabellformat, som i Excel, har du förmodligen hört talas om CSV-filer. CSV står för Comma Separated Values och är ett vanligt format för att hantera stora mängder av data. I PHP tillhandahåller CSV-funktioner en enkel och effektiv lösning för att manipulera och behandla CSV-filer.

## Hur man gör

För att kunna arbeta med CSV-filer i PHP behöver du först och främst kunna öppna och läsa filen. Detta görs med hjälp av ```fopen()``` och ```fgets()```. Här är ett enkelt exempel på hur man läser en CSV-fil och skriver ut varje rad som en tabell i HTML:

```PHP
$file = fopen("min_csv_fil.csv", "r");

echo "<table>";

while (($data = fgetcsv($file, 1000, ",")) !== FALSE) {
    echo "<tr>";
    foreach ($data as $value) {
        echo "<td>" . $value . "</td>";
    }
    echo "</tr>";
}

echo "</table>";

fclose($file);
```

Resultatet kommer att se ut som en HTML-tabell med varje cell som en separat värde från CSV-filen.

Förutom att läsa CSV-filer kan PHP också skapa nya CSV-filer och skriva data till dem. Här är ett annat exempel som visar hur man kan skapa en CSV-fil och lägga till data i den:

```PHP
$file = fopen("ny_csv_fil.csv", "w");
$data = array(
    array("Förnamn", "Efternamn", "Ålder"),
    array("Alice", "Andersson", 25),
    array("Bob", "Berg", 32),
    array("Charlotte", "Ceder", 41)
);

foreach ($data as $row) {
    fputcsv($file, $row);
}

fclose($file);
```

Detta exempel skapar en CSV-fil med tre kolumner och fyra rader med data. Du kan också lägga till mer data i filen genom att upprepa ```fputcsv()``` med fler rader.

## Djupdykning

Nu när du har en grundläggande förståelse för att läsa och skriva CSV-filer i PHP, låt oss titta på några mer avancerade funktioner som kan vara användbara när du arbetar med större och mer komplexa datamängder.

### Skippa första raden

I det första exemplet visade vi hur man läser en CSV-fil och skriver ut varje rad som en tabell. Men vad händer om den första raden i filen är en rubrikrad och inte ska skrivas ut i tabellen? För att skippa den första raden i en CSV-fil kan du använda ```fgets()``` en gång innan du börjar läsa data, som i exemplet nedan:

```PHP
$file = fopen("min_csv_fil.csv", "r");

// Skip first line
fgets($file, 1000);

echo "<table>";

while (($data = fgetcsv($file, 1000, ",")) !== FALSE) {
    echo "<tr>";
    foreach ($data as $value) {
        echo "<td>" . $value . "</td>";
    }
    echo "</tr>";
}

echo "</table>";

fclose($file);
```

### Separations tecken

I det föregående exemplet använde vi en komma (",") som separations tecken när vi läsde och skrev till CSV-filen. Men ibland kan det hända att det finns andra separatorer, som semikolon (";") eller tabb ("\t"). Du kan enkelt ändra separations tecknet genom att ange det som tredje parameter i ```fgetcsv()``` respektive ```fputcsv()```.

### Inläsning och skrivning med delade filer

När du arbetar med större datamängder kan det vara bra att dela upp filen i mindre delar, för att undvika minnesproblem. I PHP kan du använda funktionerna ```ftell()``` och ```fseek()``` för att hålla koll på var du befinner dig i en fil, så att du kan läsa och skriva från en viss position.