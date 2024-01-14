---
title:    "Bash: Att hitta längden på en sträng"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en vanlig uppgift inom Bash-programmering. Genom att förstå hur man hittar längden på en sträng kan du effektivt hantera och manipulera data i dina skript.

## Hur man gör det

För att hitta längden på en sträng i Bash kan du använda kommandot `expr length`, som tar emot en sträng som argument och returnerar dess längd. Till exempel:

```Bash
strang="Hej, världen!"
langd=$(expr length $strang)
echo "Längden på strängen är $langd" 
```

Output:

```
Längden på strängen är 14
```

Det är viktigt att notera att `expr length` bara fungerar med variabler som innehåller en enda sträng. Om du vill ta reda på längden på flera textsträngar måste du använda en loop eller ett annat verktyg som kan bearbeta flera argument.

```Bash 
namn=("Lena" "Kalle" "Nina")
for namn in ${namn[@]}; do
  langd=$(expr length $namn)
  echo "$namn är $langd bokstäver långt."
done
```

Output:

```
Lena är 4 bokstäver långt.
Kalle är 5 bokstäver långt.
Nina är 4 bokstäver långt.
```

## Djupdykning

Bash har också inbyggda variabler för att hitta längden på en sträng, utan att behöva använda `expr length`. En sådan variabel är `${#variabel}`, där "variabel" är namnet på din strängvariabel.

```Bash
strang="Hej, världen!"
echo "Längden på strängen är ${#strang}"
```

Output:

```
Längden på strängen är 14
```

Det här är ett enklare sätt att hitta längden på en sträng och kan användas med enbart en variabel.

## Se även

- [Bash: Manipulera textsträngar](https://ryanstutorials.net/bash-scripting-tutorial/bash-strings.php)
- [expr man-sida](https://www.man7.org/linux/man-pages/man1/expr.1.html)
- [Shell Skript: Regex avsnitt](https://www.shellscript.sh/regular-expressions.html)