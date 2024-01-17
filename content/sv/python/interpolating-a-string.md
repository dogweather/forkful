---
title:                "Interpolering av en sträng"
html_title:           "Python: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Interpolering av en sträng är ett sätt att bygga en ny sträng genom att ersätta variabler med specifika värden. Detta gör det möjligt för programmerare att dynamiskt skapa strängar med variabelt innehåll.

# Så här gör du:

```python
# Exempel på interpolering av en sträng med hjälp av en f-string
name = "Johan"
age = 25
print(f"Hej, jag heter {name} och är {age} år gammal.")
```

Output:
```
Hej, jag heter Johan och är 25 år gammal.
```
```python
# Exempel på interpolering av en sträng med hjälp av .format()
username = "pythonlover"
print("Din användarnamn är {}".format(username))
```

Output:
```
Ditt användarnamn är pythonlover
```

# djupdykning:

Interpolering av strängar har funnits i Python sedan version 3.6 och är ett modernare alternativ till .format(). Det finns också andra sätt att interpolera strängar, som t.ex. str.format() och % -operatorn. Det är viktigt att komma ihåg att interpolering endast fungerar med strängar, och om man vill inkludera andra datatyper måste man omvandla dem till strängar först. 

# Se även:

[Python f-string guide](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)

[Python string formatting](https://www.programiz.com/python-programming/string-interpolation)