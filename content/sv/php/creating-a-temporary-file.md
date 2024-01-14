---
title:                "PHP: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Varför
Temporary files är en viktig del av webbutveckling, speciellt när det kommer till att hantera data och filer på en server. Det är ett användbart verktyg för att temporärt lagra data eller för att utföra vissa operationer på filer innan de permanent sparas eller raderas. I denna bloggpost kommer vi att titta närmare på varför och hur man skapar temporära filer i PHP.

# Så här gör du
För att skapa en temporär fil i PHP använder man funktionen `tempnam()` som tar två parametrar - sökvägen där filen ska skapas och prefixet för filen. Om du lämnar sökvägen tom kommer filen att skapas i systemets default temporary directory. Om du vill kontrollera om filen skapades korrekt kan du använda `file_exists()` funktionen. Nedan följer ett exempel på hur man skapar en temporär fil i PHP.

```PHP
$temp_file = tempnam('/tmp', 'tmp_');

if (file_exists($temp_file)) {
  echo "Temporär fil skapad: $temp_file";
}
```

Output:
```
Temporär fil skapad: /tmp/tmp_a1b2c3
```

# Djupdykning
När en temporär fil skapas kan det vara användbart att specificera en unik prefix för filen för att undvika namnkollisioner med andra temporära filer som skapas på servern. Det är också viktigt att ta bort den temporära filen när den inte längre behövs för att undvika onödig användning av systemresurser. Detta kan göras med funktionen `unlink()` efter att du har utfört de nödvändiga operationerna på filen.

Det finns också andra användbara funktioner för att hantera temporära filer i PHP, som `sys_get_temp_dir()` för att hämta sökvägen till det temporära direktoriet som systemet använder och `tempfile()` för att snabbt skapa en temporär fil utan att behöva ange sökväg och prefix.

# Se även
- *PHP tempnam* - PHP.net (https://www.php.net/manual/en/function.tempnam.php)
- *PHP file_exists* - PHP.net (https://www.php.net/manual/en/function.file-exists.php)
- *PHP unlink* - PHP.net (https://www.php.net/manual/en/function.unlink.php)
- *PHP sys_get_temp_dir* - PHP.net (https://www.php.net/manual/en/function.sys-get-temp-dir.php)
- *PHP tempfile* - PHP.net (https://www.php.net/manual/en/function.tempfile.php)