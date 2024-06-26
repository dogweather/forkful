---
date: 2024-01-26 04:09:04.557774-07:00
description: "Jak to zrobi\u0107: Przedstawmy spos\xF3b u\u017Cycia `pdb`, wbudowanego\
  \ debugera Pythona. Wyobra\u017A sobie plik, `buggy.py`, z trudnym do zauwa\u017C\
  enia b\u0142\u0119dem."
lastmod: '2024-03-13T22:44:34.954160-06:00'
model: gpt-4-0125-preview
summary: "Przedstawmy spos\xF3b u\u017Cycia `pdb`, wbudowanego debugera Pythona."
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
Przedstawmy sposób użycia `pdb`, wbudowanego debugera Pythona. Wyobraź sobie plik, `buggy.py`, z trudnym do zauważenia błędem:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Uruchamiając ten skrypt, spodziewasz się `8`, ale otrzymujesz tylko błąd składni. Czas na debuger!

W terminalu uruchom:
```bash
python -m pdb buggy.py
```

Wejdziesz do debugera, który wygląda tak:
```Python
> /sciezka_do_pliku/buggy.py(1)<module>()
-> def add_one(number):
```

Użyj `l(ist)` aby zobaczyć więcej kodu, `n(ext)` aby przejść do następnej linii, lub `c(ontinue)` aby kontynuować uruchamianie skryptu. Kiedy natrafisz na błąd, `pdb` zatrzyma się i pozwoli ci dokonać inspekcji.

Po poprawieniu `number ++ 1` na `number + 1`, uruchom ponownie debuger, aby przetestować poprawkę.
Pamiętaj, przyjaciel nigdy nie pozwoli przyjacielowi kodować bez siatki bezpieczeństwa. Dość powiedziane.

## Pogłębiona analiza
W mrocznych czasach programowania (czyli zanim zintegrowane środowiska programistyczne, czyli IDE, stały się wszechobecne), debugery były często samodzielnymi narzędziami, których używało się na zewnątrz edytora tekstów. Przychodziły z pomocą, pozwalając programistom inspekcjonować stan ich oprogramowania w różnych punktach wykonania.

W 2023 roku, `pdb` Pythona to nie jedyna opcja na rynku. Ludzie mogą korzystać z IDE takich jak PyCharm czy Visual Studio Code, które mają wbudowane własne, wyszukane debugery. Dodają one przydatne funkcje, takie jak punkty przerwania, które można ustawić jednym kliknięciem, zamiast wpisywania krypticznych poleceń.

Jest również `ipdb`, pakiet instalowalny przez pip, który przenosi dobrodziejstwo `IPython` do debugowania. To jak `pdb` na sterydach, z autouzupełnianiem i podświetlaniem składni.

Debugery różnią się także swoją implementacją. Niektóre z nich są blisko związane z wykonaniem programu na poziomie maszynowym lub bajtkodzie. Inne, jak wiele debuggerów języków wysokiego poziomu, uruchamiają kod w specjalnym środowisku, które monitoruje stany zmiennych i kontroluje przepływ wykonania.

## Zobacz również
Aby uzyskać pełne informacje na temat debugera Pythona, sprawdź:
- Dokumentacja `pdb`: https://docs.python.org/3/library/pdb.html

Jeśli jesteś ciekaw alternatyw, te linki będą dla Ciebie pomocne:
- Repozytorium i przewodnik użytkownika `ipdb`: https://github.com/gotcha/ipdb
- Debugowanie z Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- Funkcje debugowania PyCharm: https://www.jetbrains.com/help/pycharm/debugging-code.html

Szczęśliwego polowania na błędy!
