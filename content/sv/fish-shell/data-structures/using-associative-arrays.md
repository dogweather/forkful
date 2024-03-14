---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:59.211907-07:00
description: "Associativa arrayer, eller hashmappar, l\xE5ter dig lagra data som nyckel-v\xE4\
  rdepar, vilket g\xF6r det enklare att organisera och h\xE4mta information med nyckeln.\u2026"
lastmod: '2024-03-13T22:44:38.328610-06:00'
model: gpt-4-0125-preview
summary: "Associativa arrayer, eller hashmappar, l\xE5ter dig lagra data som nyckel-v\xE4\
  rdepar, vilket g\xF6r det enklare att organisera och h\xE4mta information med nyckeln.\u2026"
title: "Att anv\xE4nda associativa arrayer"
---

{{< edit_this_page >}}

## Vad & Varför?

Associativa arrayer, eller hashmappar, låter dig lagra data som nyckel-värdepar, vilket gör det enklare att organisera och hämta information med nyckeln. De är användbara när du behöver ett mer strukturerat sätt att hantera data än bara listor, speciellt i konfigurationer och när du hanterar en rad attribut.

## Hur gör man:

Fish stöder inte native associativa arrayer som Bash 4+, men du kan uppnå liknande funktionalitet med en kombination av listor och strängmanipulation. Så här kan du efterlikna dem:

Först, ställ in "associativa array"-element separat:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

För att komma åt ett element, referera till det direkt:

```Fish Shell
echo $food_color_apple
# Utdata: röd
```

Om du behöver iterera över dem, använd en for-loop som överväger en namnkonvention:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Utdata:
# röd
# gul
```

För de som saknar Bashs `${!array[@]}` för att få alla nycklar, kan du lagra nycklar i en separat lista:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'är' $food_color_$key
end
# Utdata:
# apple är röd
# banana är gul
```

## Fördjupning

Riktiga associativa arrayer som i andra skriptspråk är ännu inte en del av Fishs ansats. Lösningen som visas utnyttjar Fishs strängmanipulation och listkapaciteter för att skapa en pseudo-associativ arraystruktur. Även om det fungerar, är det inte lika rent eller felfritt som inbyggt stöd för associativa arrayer skulle vara. Andra skal som Bash och Zsh erbjuder inbyggd funktionalitet för associativa arrayer, vilket resulterar i mer rakt på sak, läsbart kod. Dock syftar Fishs designfilosofi till enkelhet och användarvänlighet, möjligtvis på bekostnad av sådana funktioner. Genvägen tillfredsställer de flesta behov, men håll ett öga på Fish Shells utveckling—dess utvecklare förbättrar och lägger till funktioner baserat på gemenskapsfeedback.
