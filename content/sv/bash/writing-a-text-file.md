---
title:                "Skriva en textfil"
html_title:           "Bash: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil kan vara ett användbart verktyg för att dokumentera och spara processer, kommandon och annan information inom Bash-programmering. Det är en lättillgänglig och enkel metod för att organisera och återanvända kod.

## Hur man gör

Att skapa en textfil i Bash är enkel. Du behöver bara öppna ett terminalfönster och skriva in kommandot "touch" följt av namnet på din textfil. Detta kommer att skapa en tom fil. Sedan kan du använda kommandot "echo" för att skriva in text i filen. Till exempel:

```Bash
touch min_textfil.txt
echo "Det här är en textfil som jag skapat" > min_textfil.txt

KÖR:

cat min_textfil.txt
```
Output:
Det här är en textfil som jag skapat

## Deep Dive

Det finns flera andra sätt att skapa och redigera textfiler i Bash. Du kan använda kommandot "cat" för att visa innehållet i en textfil. Med hjälp av "> >" kan du lägga till text i en befintlig fil utan att skriva över den befintliga texten. Om du vill visa innehållet från flera filer i en enda output, kan du använda "cat" tillsammans med wildcard-tecknet "*". Till exempel:

```Bash
cat min_textfil.txt en_annan_textfil.txt > output.txt
```
I detta exempel kommer innehållet från både "min_textfil.txt" och "en_annan_textfil.txt" att visas i den nya filen "output.txt".

## Se även

- [Linux Bash-kommandon](https://www.eurovps.com/blog/15-most-used-linux-commands/)
- [Bash-skript för att automatisera uppgifter](https://www.tecmint.com/execute-shell-scripts-using-bash-syntax/)
- [Guide för att använda textfiler i Bash](https://www.digitalocean.com/community/tutorials/how-to-use-text-files-in-bash-scripts)