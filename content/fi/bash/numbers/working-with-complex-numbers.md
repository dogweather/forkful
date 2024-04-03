---
date: 2024-01-26 04:37:01.168827-07:00
description: "Kuinka: Bash ei tue kompleksilukuja natiivisti. Usein k\xE4yt\xE4t ulkoista\
  \ ty\xF6kalua kuten `bc` sen `-l`-vaihtoehdon kanssa. N\xE4in k\xE4sittelet kompleksilukuja\u2026"
lastmod: '2024-03-13T22:44:56.731689-06:00'
model: gpt-4-0125-preview
summary: Bash ei tue kompleksilukuja natiivisti.
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
Bash ei tue kompleksilukuja natiivisti. Usein käytät ulkoista työkalua kuten `bc` sen `-l`-vaihtoehdon kanssa. Näin käsittelet kompleksilukuja bashissa:

```bash
echo "sqrt(-1)" | bc -l
```

Tuloste:
```bash
j
```

Kertolasku:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Tuloste:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Syväsukellus
Kompleksiluvut ovat olleet olemassa jo 16. vuosisadalta lähtien, mutta käsikirjoituskielet kuten Bash eivät ole suoraan tarkoitettu matemaattisiin laskutoimituksiin kuten kompleksiluvut. Siksi usein `bc` tai muita työkaluja kuten `awk` tulee tarpeeseen. Jotkin vaihtoehtoiset kielet kompleksilukujen käsittelyyn ovat Python sen `cmath`-moduulin kanssa ja MATLAB, jotka molemmat on rakennettu monimutkaisempia matemaattisia toimintoja varten. Bashin osalta kyse on työkalujen hyödyntämisestä - `bc` käyttää pientä 'i':tä edustamaan imaginaariyksikköä ja tukee perusoperaatioita kuten yhteenlaskua, vähennyslaskua, kertolaskua ja jakolaskua.

## Katso myös
- `bc`:n ohjekirja: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (vaihtoehto MATLABille): https://www.gnu.org/software/octave/
- Python `cmath`-moduuli: https://docs.python.org/3/library/cmath.html
