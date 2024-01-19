---
title:                "Gör om en sträng till versaler"
html_title:           "Python: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför? 
Att omvandla en sträng (string) till versaler (helt stora bokstäver) kan i programmering kallas att "capitalizing". Vi gör det för att forma och standardisera data, så att exempelvis användarnamn eller mailadresser tillåter både koppling och variation, men normalt inte skiljer mellan stora och små bokstäver.

## Hur man gör: 
Här är exempel på hur du gör för att konvertera sträng till versaler i Python:

```Python 
text = 'jag älskar python programmering'
capitalized_text = text.upper()
print(capitalized_text)
```
Output:
``` 
JAG ÄLSKAR PYTHON PROGRAMMERING
```

## Djupdykning 
I gamla programmeringsspråk var stora och små bokstäver mycket viktiga. Exempelvis i språket C måste du skriva all kod med små bokstäver. I moderna språk, inklusive Python, finns funktioner som autokonverterar strängar. 

Alternativ till metoden .upper() är metoden .capitalize(), som bara gör den första bokstaven i strängen stor:

```Python
text = 'jag älskar python programmering'
capitalized_text = text.capitalize()
print(capitalized_text)
```
Output: 
```
Jag älskar python programmering
```

Om du vill ha vart ord med stor bokstav, kan du använda .title():

```Python
text = 'jag älskar python programmering'
capitalized_text = text.title()
print(capitalized_text)
```
Output: 
```
Jag Älskar Python Programmering
```

I praktiken beror det vilken metod du ska använda på vad du vill uppnå.

## Se också 
För ytterligare information, se följande länkar:
1. Python Dokumentation: Strängmetoder - [Länk](https://docs.python.org/3/library/stdtypes.html#string-methods)
2. W3Schools Python Strängmetoder - [Länk](https://www.w3schools.com/python/python_ref_string.asp)