---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:27.711251-07:00
description: "Hur man g\xF6r: Att skapa en dictionary i Python \xE4r rakt p\xE5 sak.\
  \ Du omsluter nyckel-v\xE4rdepar i m\xE5svingar `{}`, med nycklar och v\xE4rden\
  \ separerade av ett kolon."
lastmod: '2024-03-13T22:44:37.476269-06:00'
model: gpt-4-0125-preview
summary: "Att skapa en dictionary i Python \xE4r rakt p\xE5 sak."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
Att skapa en dictionary i Python är rakt på sak. Du omsluter nyckel-värdepar i måsvingar `{}`, med nycklar och värden separerade av ett kolon:

```Python
# Skapa en associativ array (dictionary)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Utdata:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Att komma åt ett värde med dess nyckel är enkelt:

```Python
# Kom åt ett värde
print(my_dict["name"])
```

Utdata:
```
John
```

Att lägga till eller uppdatera element görs genom att tilldela ett värde till en nyckel:

```Python
# Lägg till ett nytt nyckel-värdepar
my_dict["email"] = "john@example.com"
# Uppdatera ett värde
my_dict["age"] = 31
print(my_dict)
```

Utdata:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

För att iterera över dictionary-element:

```Python
# Iterera genom nyckel-värdepar
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Utdata:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Fördjupning
Associativa arrayer i Python, eller dictionaries, introducerades för att tillhandahålla en datastruktur för effektiv dataåtkomst och manipulation. I motsats till sekvenser, som är indexerade med en rad nummer, indexeras dictionaries med nycklar, vilka kan vara av vilken oföränderlig typ som helst. Detta designval gör dictionaries idealiska för snabba uppslagstabeller där nycklar kopplar till unika värden.

Historiskt sett har Python-dictionaries implementerats med hjälp av en hashtabell, vilket säkerställer att den genomsnittliga tidskomplexiteten för uppslag, tillägg och borttagning är O(1). Från och med Python 3.6 och senare, bibehåller dictionaries även införingsordningen för poster, vilket kombinerar fördelarna med hashtabeller med förutsägbarheten hos införingsordningen som ses i ordnade datastrukturer.

Även om dictionaries är otroligt mångsidiga, kan i vissa specialiserade fall, alternativ som `collections.defaultdict` eller `collections.OrderedDict` (före Python 3.7) vara att föredra. `defaultdict` är särskilt användbart när du behöver en dictionary för att returnera ett standardvärde för icke-existerande nycklar, vilket förenklar vissa typer av villkorslogik. Dock, med den kontinuerliga förbättringen och utvecklingen av Python, förblir den inbyggda dictionary-klassen ofta det förstahandsvalet för associativa arrayer på grund av dess robusthet och den bekvämlighet den erbjuder direkt ur lådan.
