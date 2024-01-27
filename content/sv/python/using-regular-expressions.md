---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Regular expressions (regex) hjälper till att söka och manipulera strängar baserat på mönster. Programmerare använder regex för att effektivisera textbearbetning, validering och dataextraktion.

## Hur gör man:
```python
import re

# Hitta alla orden som börjar med 'b' och följs av en vokal
text = "Baren är öppen, bära eller byta?"
mönster = r"\bb[aeiouyåäö]\w+"
hittade_ord = re.findall(mönster, text, re.IGNORECASE)
print(hittade_ord)  # Output: ['Baren', 'bära', 'byta']
```

```python
# Validera ett svenskt personnummer
personnummer = "850709-1234"
validera_pnr = r"^\d{6}-\d{4}$"
match = re.fullmatch(validera_pnr, personnummer)
print('Giltigt' if match else 'Ogiltigt')  # Output: Giltigt
```

## Djupdykning
Regex härstammar från 1950-talets teoretiska arbete med formella språk. Alternativ till regex inkluderar strängmetoder som `find()` eller bibliotek som `string`. Python implementerar regex via `re`-modulen, som använder en bakåtkompatibel variant av Perl's regex-motor.

## Se även
- Python's `re` modul i dokumentation: https://docs.python.org/3/library/re.html
- RegExr, för att experimentera med regex online: https://regexr.com/
- Regex101, med stöd för python-syntax: https://regex101.com/
