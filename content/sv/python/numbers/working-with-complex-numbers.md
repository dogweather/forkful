---
title:                "Att arbeta med komplexa tal"
aliases:
- /sv/python/working-with-complex-numbers.md
date:                  2024-01-26T04:44:51.433838-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med komplexa tal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Vad och varför?
Komplexa tal är en mängd tal i formen `a + bi`, där `a` och `b` är reella tal, och `i` är den imaginära enheten (`i^2 = -1`). I programmering använder vi dem för att lösa problem inom olika områden, som elektroteknik, signalbehandling och kvantdatorberäkning.

## Hur man gör:
Python har inbyggt stöd för komplexa tal. Så här kan du leka med dem:

```Python
# Skapa komplexa tal
z = 4 + 5j
print(z)  # Utdata: (4+5j)

# Komma åt real- och imaginärdelar
print(z.real)  # Utdata: 4.0
print(z.imag)  # Utdata: 5.0

# Komplex aritmetik
w = 1 - 2j
print(z + w)  # Utdata: (5+3j)
print(z - w)  # Utdata: (3+7j)
print(z * w)  # Utdata: (14+2j)
print(z / w)  # Utdata: (-3.6+1.2j)

# Absolutbelopp (modulus)
print(abs(z))  # Utdata: 6.4031242374328485

# Konjugat av ett komplext tal
print(z.conjugate())  # Utdata: (4-5j)
```

## Fördjupning
Komplexa tal konceptualiserades först av Gerolamo Cardano under 1500-talet. Python, bland andra programmeringsspråk, behandlar komplexa tal som förstaklassens medborgare. Det betyder att de är inbyggda i språket, med lättanvända funktioner, och undviker behovet av att importera externa bibliotek för grundläggande operationer.

Dock, för tunga numeriska beräkningar, har Python ett bibliotek som heter `cmath`, vilket är specifikt för komplexa tal. Det har ytterligare funktioner som `exp`, `log` och trigonometriska operationer.

När Python inte räcker till kanske du vänder dig till bibliotek som NumPy, särskilt för arrayoperationer som involverar komplexa tal. NumPy erbjuder optimerade och vektoriserade operationer som är avgörande för prestanda inom numerisk beräkning.

## Se även
Kolla in dessa resurser för att lära dig mer:

- Pythons officiella dokumentation om komplexa tal: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- Dokumentationen för `cmath`-modulen: https://docs.python.org/3/library/cmath.html
- NumPy för hantering av arrayer med komplexa tal: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
