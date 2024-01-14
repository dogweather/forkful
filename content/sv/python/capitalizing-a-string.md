---
title:    "Python: Göra en sträng med stora bokstäver"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att göra bokstäver stora är en vanlig operation som används för att förbättra läsbarheten eller för att hålla en enhetlig stil i texten. Det kan också vara en nödvändig åtgärd när man arbetar med data och vill säkerställa att alla stavningsregler följs. Oavsett anledning kan du enkelt göra detta med hjälp av Python.

## Hur man gör

För att göra en sträng till stora bokstäver i Python kan du använda metoden `upper()` som kan användas på alla strängar. Till exempel, om vi har en variabel `namn` med värdet "anna", skulle detta vara koden för att få det som "ANNA":

```python
namn = "anna"
print(namn.upper())
```

Output:

```
ANNA
```

En annan användbar metod är `title()` som gör om den första bokstaven i varje ord till stor bokstav. Om vi använder detta på samma variabel `namn`, skulle resultatet bli "Anna":

```python
print(namn.title())
```

Output:

```
Anna
```

Det finns också en funktion `capitalize()` som endast gör första bokstaven i en sträng stor. Med vårt exempel skulle funktionen `capitalize()` returnera "Anna":

```python
print(namn.capitalize())
```

Output:

```
Anna
```

Du kan också använda formatmallar för att göra en sträng till stora bokstäver och välja vilka bokstäver du vill visa i storlek. Till exempel, om vi har en sträng med flera ord och bara vill ha stora bokstäver för det första och sista ordet, kan du göra detta:

```python
strang = "det här är en sträng med flera ord"
print(f"{strang.split()[0].upper()} {strang.lower().split()[-1]}")
```

Output:

```
DET här är en sträng med FLERA ord
```

## Djupdykning

Innan vi dyker djupare in i hur man gör stora bokstäver i Python, är det viktigt att förstå skillnaden mellan metoden `upper()` och funktionen `capitalize()`. `upper()` gör om alla bokstäver i en sträng till stora bokstäver, medan `capitalize()` endast gör om den första bokstaven i strängen till stor bokstav och lämnar resten av strängen som den är.

En annan detalj att notera är att båda dessa metoder är fallöverkänsliga, vilket betyder att de kommer att göra om även små bokstäver som redan är stora till stora bokstäver. Så om du redan har en sträng i stora bokstäver och använder `upper()` på den, kommer resultatet fortfarande att vara i stora bokstäver.

Nu när du har en grundläggande förståelse för hur man gör stora bokstäver i Python, kan du börja experimentera med olika sätt att implementera det i ditt eget kodprojekt.

## Se även

Här är några användbara länkar för att lära dig mer om hur man arbetar med strängar i Python:

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [An Intro to String Manipulation in Python 3](https://realpython.com/python-strings/)

Tack för att du läste denna artikel om hur man gör stora bokstäver i Python!