---
date: 2024-01-26 04:37:01.168827-07:00
description: "Kompleksiluvut koostuvat reaalisesta osasta ja imaginaarisesta osasta.\
  \ Ohjelmoijat k\xE4ytt\xE4v\xE4t niit\xE4 aloilla kuten signaalink\xE4sittely, kvanttimekaniikka\
  \ ja\u2026"
lastmod: '2024-03-13T22:44:56.731689-06:00'
model: gpt-4-0125-preview
summary: "Kompleksiluvut koostuvat reaalisesta osasta ja imaginaarisesta osasta. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t niit\xE4 aloilla kuten signaalink\xE4sittely, kvanttimekaniikka\
  \ ja\u2026"
title: "Kompleksilukujen k\xE4sittely"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Kompleksiluvut koostuvat reaalisesta osasta ja imaginaarisesta osasta. Ohjelmoijat käyttävät niitä aloilla kuten signaalinkäsittely, kvanttimekaniikka ja aina kun laskelmat vaativat, koska tavalliset reaaliluvut eivät yksinkertaisesti riitä.

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
