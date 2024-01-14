---
title:                "Python: Utmatning av felsökningsinformation"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/printing-debug-output.md"
---

{{< edit_this_page >}}

##Varför
Att skriva programspråk kan vara en komplex och tidskrävande process. Ibland kan det uppstå buggar eller fel i koden som är svåra att hitta och åtgärda. Genom att använda tryckning av felsökningstext kan man enkelt spåra dessa problem och snabbare lösa dem.

##Så här
För att skriva ut felsökningstext i Python kan du använda funktionen `print`. Du kan skriva ut olika typer av information såsom variabler, strängar och utskrift av olika steg i ditt program. Ett exempel på kodblock kan se ut så här:

```Python
num1 = 10
print("Det första numret är:", num1)
```

Detta kommer att ge följande utmatning:

```
Det första numret är: 10
```

Du kan också lägga till flera variabler och strängar inuti `print` funktionen genom att använda kommatecken mellan dem. Detta kommer att skriva ut varje element åtskilt med ett mellanslag.

```Python
num2 = 7.5
print("Det andra numret är:", num2, "och summan av numren är:", num1 + num2)
```

Detta ger följande utmatning:

```
Det andra numret är: 7.5 och summan av numren är: 17.5
```

##Djupdykning
När det gäller att skriva ut felsökningstext finns det flera andra funktioner som kan hjälpa till att göra processen ännu enklare. Till exempel kan du använda `format` funktionen för att formatera utskriften av ditt felsökningstext. Du kan också använda `repr` funktionen för att få en mer detaljerad utmatning av dina objekt.

En annan användbar funktion är `sys` modulen, som ger tillgång till systemrelaterad information som till exempel vilken plattform ditt program körs på och vilken version av Python som används. Du kan använda `sys.stderr` för att skriva ut felmeddelanden och `sys.stdout` för att skriva ut annan felsökningstext.

##Se också
- [7 sätt att använda Python för felsökning](https://www.datacamp.com/community/tutorials/7-essential-python-scripts-debugging)
- [Felsökningsguide för Python-utvecklare](https://realpython.com/python-debugging-pdb/)
- [Python dokumentationen för `print`](https://docs.python.org/3/library/functions.html#print)