---
date: 2024-01-26 00:57:13.390668-07:00
description: "Hur man g\xF6r: Exempel p\xE5 utdata n\xE4r man matar in ett ogiltigt\
  \ nummer f\xF6r den f\xF6rsta delen."
lastmod: '2024-04-05T21:53:38.816815-06:00'
model: gpt-4-1106-preview
summary: "Exempel p\xE5 utdata n\xE4r man matar in ett ogiltigt nummer f\xF6r den\
  \ f\xF6rsta delen."
title: Hantering av fel
weight: 16
---

## Hur man gör:
``` Python
# Grundläggande try-except-block
try:
    # riskfylld kod
    number = int(input("Ange ett nummer: "))
except ValueError:
    # hantera fel
    print("Det där är inte ett nummer!")

# Specificera flera undantag
try:
    # kod som kan orsaka olika undantag
    resultat = 10 / int(input("Ange en divisor: "))
except ZeroDivisionError:
    print("Hoppsan! Kan inte dela med noll.")
except ValueError:
    print("Jag behöver ett nummer, kompis.")

# Använda else och finally
try:
    number = int(input("Ange ett nummer för kvadrering: "))
except ValueError:
    print("Jag sa ett nummer!")
else:
    # inga fel uppstod
    print("Ditt nummer i kvadrat är:", number**2)
finally:
    # exekveras alltid
    print("Tack för att du testade detta!")
```

Exempel på utdata när man matar in ett ogiltigt nummer för den första delen:
```
Ange ett nummer: hej
Det där är inte ett nummer!
```

## Fördjupning
Sedan gryningen av programmering har felhantering varit avgörande. Tidiga metoder var grundläggande, som att kontrollera villkor före varje riskabelt operation. Pythons `try-except` syntax härstammar från en tradition av undantagshantering i äldre språk som C++ och Java, vilket förenklar processen.

När du `try` ett block av kod, håller Python utkik efter eventuella undantag. Om ett fel uppkommer, fångar `except`-blocket det. Du kan vara specifik om vilka undantag du fångar eller fånga 'em alla med ett bart `except`. Men, att vara specifik först är en bättre strategi - det är mer precist, inte ett fångstnät för allt.

`else` och `finally` är extra tillägg i detta koncept. `else`-blocket körs om try-blocket är felfritt. `finally` är den pålitliga kompisen som körs oavsett vad - tänk städuppgifter.

Alternativ? Det finns säkert. Vissa språk använder återgångskoder istället för undantag. Du kan också stöta på `with`-satsen för hantering av resurser eller `assertions` som kontrollerar villkor under utveckling. Men när vi talar om solida strategier för felhantering, står try-catch-modellen ut för sin läsbarhet och struktur.

## Se även
Här är några bra ytterligare resurser för att fördjupa dig ännu mer:

- Pythons officiella dokumentation om fel och undantag: [Python Docs – Errors and Exceptions](https://docs.python.org/3/tutorial/errors.html)
- Real Pythons guide i ämnet: [Real Python - The try/except/else/finally block](https://realpython.com/python-exceptions/)
- En tankeväckande diskussion om bästa praxis för felhantering: [Stack Overflow – How do I properly ignore exceptions?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
