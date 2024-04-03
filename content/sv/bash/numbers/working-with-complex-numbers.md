---
date: 2024-01-26 04:37:10.079707-07:00
description: "Hur: Bash st\xF6djer inte komplexa tal av sig sj\xE4lvt. Du kommer ofta\
  \ att anv\xE4nda ett externt verktyg som `bc` med dess `-l` alternativ. S\xE5 h\xE4\
  r kn\xE4cker du\u2026"
lastmod: '2024-03-13T22:44:38.074515-06:00'
model: gpt-4-0125-preview
summary: "Bash st\xF6djer inte komplexa tal av sig sj\xE4lvt."
title: Att arbeta med komplexa tal
weight: 14
---

## Hur:
Bash stödjer inte komplexa tal av sig självt. Du kommer ofta att använda ett externt verktyg som `bc` med dess `-l` alternativ. Så här knäcker du komplexa tal i bash:

```bash
echo "sqrt(-1)" | bc -l
```

Utdata:
```bash
j
```

Multiplikation:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Utdata:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Fördjupning
Komplexa tal har funnits sedan 1500-talet, men skriptspråk som Bash är inte förberedda för matematiska beräkningar som komplexa tal direkt från start. Det är därför `bc` eller andra verktyg såsom `awk` ofta kommer till användning. Några alternativa språk för att arbeta med komplexa tal är Python med dess `cmath` modul och MATLAB, som båda är byggda för mer avancerade matematiska funktioner. När det gäller Bash, handlar det allt om att utnyttja verktyg - `bc` använder den lilla 'i' för att representera den imaginära enheten och stöder grundläggande operationer såsom addition, subtraktion, multiplikation och division.

## Se även
- `bc` manualen: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternativ för MATLAB): https://www.gnu.org/software/octave/
- Python `cmath` modulen: https://docs.python.org/3/library/cmath.html
