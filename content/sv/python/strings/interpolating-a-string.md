---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:23:40.973933-07:00
description: "Str\xE4nginterpolering \xE4r metoden att b\xE4dda in uttryck inom str\xE4\
  ngliteraler. Programmerare anv\xE4nder det f\xF6r att dynamiskt infoga v\xE4rden\
  \ i str\xE4ngar, vilket\u2026"
lastmod: '2024-03-11T00:14:10.779693-06:00'
model: gpt-4-0125-preview
summary: "Str\xE4nginterpolering \xE4r metoden att b\xE4dda in uttryck inom str\xE4\
  ngliteraler. Programmerare anv\xE4nder det f\xF6r att dynamiskt infoga v\xE4rden\
  \ i str\xE4ngar, vilket\u2026"
title: "Interpolering av en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Stränginterpolering är metoden att bädda in uttryck inom strängliteraler. Programmerare använder det för att dynamiskt infoga värden i strängar, vilket gör koden mer läslig och renare än traditionell strängkonkatenering.

## Hur:
I Python 3.6 och senare kan du interpolera strängar genom att använda f-strängar. Så här gör du:

```Python
name = 'Alice'
age = 30
greeting = f"Hej, {name}. Du är {age} år gammal."

print(greeting)
```

Utskrift:
```
Hej, Alice. Du är 30 år gammal.
```

Du kan också använda uttryck inuti klammerparenteserna:

```Python
a = 5
b = 10
info = f"Fem plus tio är {a + b}, inte {2 * (a + b)}."

print(info)
```

Utskrift:
```
Fem plus tio är 15, inte 30.
```

## Djupdykning
Innan Python 3.6 var `.format()` vägen att gå för att interpolera strängar:

```Python
name = 'Bob'
age = 25
greeting = "Hej, {}. Du är {} år gammal.".format(name, age)

print(greeting)
```

Gammal skolans Python (versioner < 2.6) använde `%`-operatorn för interpolation, vilket är mindre intuitivt och kan bli rörigt med flera variabler:

```Python
name = 'Carol'
age = 35
greeting = "Hej, %s. Du är %d år gammal." % (name, age)

print(greeting)
```

Förutom renare syntax, är f-strängar snabbare eftersom de utvärderas vid körning och sedan omvandlas direkt till en effektiv strängformateringsoperation. `.format()` och `%`-operatorn involverar fler steg och är långsammare.

## Se även
- [PEP 498 – Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/) för officiell dokumentation om f-strängar.
- [Python f-strängar](https://realpython.com/python-f-strings/) av Real Python för en handledning om användning av f-strängar.
- [Metoden .format()](https://docs.python.org/3/library/stdtypes.html#str.format) i Python-dokumentationen för att förstå den äldre `.format()`-metoden för strängformatering.
