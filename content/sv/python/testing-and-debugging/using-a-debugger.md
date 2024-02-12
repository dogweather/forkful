---
title:                "Att använda en debugger"
aliases:
- /sv/python/using-a-debugger/
date:                  2024-01-26T04:08:55.145628-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad & Varför?
"Att använda en debugger" innebär att stega igenom din Python-kod för att hitta buggar och förstå beteenden. Vi gör det eftersom det är mycket enklare än att bara gissa var saker har gått fel, och det sparar oss timmar av primitiv felsökning med print-satser.

## Hur man gör:
Låt oss bryta ner användningen av `pdb`, Pythons inbyggda debugger. Föreställ dig en fil, `buggy.py`, med en listig bugg:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

När du kör detta skript, förväntar du dig `8`, men det kastar bara ett syntaxfel. Det är dags för debuggern!

I din terminal, kör:
```bash
python -m pdb buggy.py
```

Du kommer att komma in i debuggern, och det ser ut så här:
```Python
> /sökväg_till_fil/buggy.py(1)<module>()
-> def add_one(number):
```

Använd `l(ist)` för att se mer kod, `n(ext)` för att gå till nästa rad, eller `c(ontinue)` för att fortsätta köra skriptet. När du når felet, kommer `pdb` att stoppa och låta dig inspektera.

Efter att du rättat till `number ++ 1` till `number + 1`, starta om debuggern för att testa lösningen.
Kom ihåg, vänner låter inte vänner koda utan ett nät. Nog sagt.

## Fördjupning
Tillbaka i programmeringens mörka åldrar (a.k.a. innan integrerade utvecklingsmiljöer, eller IDEer, var överallt), var debuggers ofta fristående verktyg som du skulle använda utanför din textredigerare. De kom till undsättning genom att låta programmerare inspektera tillståndet på deras mjukvara vid olika exekveringspunkter.

Från och med 2023 är Pythons `pdb` inte den enda möjligheten. Folk kanske använder IDEer som PyCharm eller Visual Studio Code, som har sina egna eleganta debuggers inbyggda. Dessa lägger till praktiska funktioner som brytpunkter som du kan sätta med ett klick, istället för att skriva kryptiska kommandon.

Sedan finns det `ipdb`, ett pip-installationsbart paket som för med sig `IPython`-godheten till debugging. Det är som `pdb` på prestandafrämjande medel, med tab-komplettering och syntaxmarkering.

Debuggers varierar också i deras implementation. Vissa går djupt in i programexekveringen på maskin- eller bytekodsnivå. Andra, som många debuggers för högnivåspråk, kör koden i en särskild miljö som övervakar variabeltillstånd och kontrollerar exekveringsflödet.

## Se även
För full information om Pythons egen debugger, kolla in:
- `pdb`-dokumentationen: https://docs.python.org/3/library/pdb.html

Om du är nyfiken på alternativ, kommer dessa länkar att vara till hjälp:
- `ipdb`-repository och användarguide: https://github.com/gotcha/ipdb
- Debugging med Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- PyCharms debuggingfunktioner: https://www.jetbrains.com/help/pycharm/debugging-code.html

Lycklig buggjakt!
