---
title:                "Bash: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Varför
Det finns många gånger i programmering när man behöver kontrollera om en viss mapp finns eller inte. Det kan till exempel vara för att undvika att skriva över befintliga filer eller för att navigera i ett stort projektstruktur. Att känna till hur man gör detta är en viktig del av en vardaglig Bash-programmering.

# Hur man gör det
Det finns flera olika sätt att kontrollera om en mapp finns i Bash. Ett enkelt sätt är att använda kommandot `if`, följt av `[-d <mappen>]` för att kolla om mappen existerar eller inte. Detta kan se ut såhär:
```Bash
if [-d /hem/Kalle/mapp]; then
  echo "Mappen finns!"
else
  echo "Mappen finns inte."
fi
```
Detta kodblock kommer att kontrollera om mappen "mapp" finns i sökvägen `/hem/Kalle/`. Om mappen finns kommer ett meddelande om det att skrivas ut, annars skrivs ett annat meddelande ut. Det är viktigt att observera att en mellanslag måste finnas mellan `[` och `-d` för att det ska fungera.

# Djupdykning
För de som vill veta mer om hur man kontrollerar mappar i Bash finns det flera andra metoder. Till exempel kan man använda `test`-kommandot istället för `if -d` för att göra samma sak. Test-kommandot är mer allmänt och kan också användas för att kontrollera andra typer av filer, som textfiler eller binärfiler.

En annan användbar teknik är att använda wildcard-tecknet `*` för att kontrollera innehållet i en mapp. Om man till exempel bara vill kontrollera om en mapp innehåller en viss fil istället för att kontrollera hela mappstrukturen kan man skriva `[ -f /hem/Kalle/mapp/* ]`, där `*` står för alla filer i mappen. Detta kommer att returnera sant om någon av filerna matchar och falskt om inga filer matchar.

# Se även
- [Bash if-sats](https://www.linuxcast.se/tips/bash-if-sats/)
- [Bash test-kommandon](https://linuxize.com/post/bash-test-command/)
- [Bash wildcard-tecken](https://www.gnu.org/software/bash/manual/html_node/Wildcards.html)