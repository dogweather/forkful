---
date: 2024-01-26 04:37:10.079707-07:00
description: "Komplexa tal best\xE5r av en reell del och en imagin\xE4r del. Programmerare\
  \ anv\xE4nder dem inom omr\xE5den s\xE5som signalbehandling, kvantmekanik och n\xE4\
  rhelst\u2026"
lastmod: '2024-02-25T18:49:36.389449-07:00'
model: gpt-4-0125-preview
summary: "Komplexa tal best\xE5r av en reell del och en imagin\xE4r del. Programmerare\
  \ anv\xE4nder dem inom omr\xE5den s\xE5som signalbehandling, kvantmekanik och n\xE4\
  rhelst\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal består av en reell del och en imaginär del. Programmerare använder dem inom områden såsom signalbehandling, kvantmekanik och närhelst uträkningen kräver det, eftersom vanliga reella tal helt enkelt inte är tillräckliga.

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
