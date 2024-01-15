---
title:                "Analiza składni HTML"
html_title:           "Fish Shell: Analiza składni HTML"
simple_title:         "Analiza składni HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli kiedykolwiek musiałeś (lub będziesz musiał) wydobywać pewne informacje z dużej ilości danych w formacie HTML, wtedy wiesz, jak dokuczliwe i czasochłonne może to być ręczne przeszukiwanie i kopiowanie. Tutaj z pomocą przychodzi programowanie w języku Fish Shell! Pozwala ono na automatyczne parsowanie i wyodrębnianie potrzebnych danych z plików HTML, co może znacznie przyspieszyć i ułatwić pracę.

## Jak to zrobić

Zanim przejdziemy do przykładów kodu, trzeba zauważyć, że Fish Shell nie jest domyślnie wyposażony w funkcje do parsowania HTML. Jednak dzięki możliwościom dostępnym w języku UNIX, możemy skorzystać z narzędzi takich jak `curl` i `sed` do pobrania i przetworzenia danych z pliku HTML.

W poniższych przykładach będziemy analizować stronę internetową z listą ulubionych przepisów kulinarnych i wyodrębniać nazwy potraw oraz składniki. Zwróć uwagę, że przykłady są uproszczone i opierają się na założeniu, że plik HTML jest odpowiednio sformatowany.

```Fish Shell
# Pobranie zawartości strony internetowej przy użyciu narzędzia curl i przekierowanie jej do pliku o nazwie "lista_przepisow.html"
curl https://www.example.com/lista_przepisow > lista_przepisow.html

# Wyodrębnienie nazw potraw 
cat lista_przepisow.html | sed -n 's/<h3>\(.*\)<\/h3>/\1/p' > nazwy_potraw.txt

# Wyodrębnienie składników 
cat lista_przepisow.html | sed -n 's/<li>\(.*\)<\/li>/\1/p' > skladniki.txt

# Wyświetlenie wyodrębnionych danych
cat nazwy_potraw.txt
cat skladniki.txt
```

**Rezultat:**

Nazwy potraw:
```
Pierogi z mięsem
Kotlet schabowy
Sałatka grecka
Spaghetti bolognese
```

Składniki:
```
Ciasto na pierogi, mięso mielone, cebula, jajko
Mięso schabowe, bułka tarta, jajko, sól, pieprz
Ogórki, pomidory, cebula, ser feta, oliwki
Makaron spaghetti, mięso mielone, pomidory, cebula, czosnek, przyprawy
```

Jeśli chcemy wyodrębnić inne informacje z pliku HTML, wtedy wystarczy dostosować wyrażenia regularne używane w poleceniach `sed` do naszych potrzeb.

## Przykłady wykorzystania

### Skrypt do statystyk

Załóżmy, że mamy plik HTML ze spisem cen naszych ulubionych produktów. Chcemy wyodrębnić tylko ceny i obliczyć średnią oraz maksymalną wartość.

```Fish Shell
# Pobranie zawartości strony internetowej przy użyciu narzędzia curl i przekierowanie jej do pliku o nazwie "spis_cen.html"
curl https://www.example.com/spis_cen > spis_cen.html

# Wyodrębnienie cen i zapisanie ich do pliku tymczasowego
cat spis_cen.html | sed -n 's/<span class="price">\(.*\)<\/span>/\1/p' > tmp_ceny.txt

# Obliczenie średniej i maksymalnej ceny przy użyciu wbudowanych funkcji Fish Shell
cat tmp_ceny.txt | awk '{sum+=$1} END{print "Średnia cena: "sum/NR"\nMaksymalna cena