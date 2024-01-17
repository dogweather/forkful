---
title:                "Generowanie losowych liczb"
html_title:           "Fish Shell: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Co to jest i dlaczego?

Generowanie losowych liczb jest procesem tworzenia liczby lub ciągu liczb w sposób pseudolosowy. Programiści często wykorzystują ten proces do tworzenia symulacji, gier lub do generowania unikalnych identyfikatorów. 

# Jak to zrobić:

Według kanonicznej definicji, musimy użyć funkcji `random` w celu wygenerowania losowych liczb. Oto przykładowy kod w języku Fish Shell, który wyświetli 10 losowych liczb od 1 do 100:

```fish
for i in (seq 1 10)
	echo (random 1 100)
end
```

Po uruchomieniu powyższego kodu, otrzymamy wynik podobny do tego:

```fish
82.934609
10.48204 
71.350336 
95.155582 
63.741849 
69.527023 
54.524158 
16.843962 
68.924905 
4.6280007
```

# Wgląd w szczegóły:

Historia generowania losowych liczb sięga czasów starożytnych, gdzie używano różnych metod, takich jak rzut monetą czy obracanie kołem. Współczesne komputery wykorzystują algorytmy generowania liczb pseudolosowych, które wykorzystują dane dostarczane przez użytkownika, takie jak czas lub pozycja kursora.

Alternatywne metody generowania losowych liczb to m.in. użycie rzutu kostkami, wykorzystanie danych zewnętrznych (np. pogoda), czy wykorzystanie fizycznych zjawisk, takich jak efekt kolizji.

Implementacja generowania losowych liczb jest bardzo ważna, ponieważ źle napisany algorytm może doprowadzić do powtarzających się wzorców i wpływać na losowość liczb.

# Zobacz też:

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Generowanie liczb pseudolosowych - Wikipedia](https://pl.wikipedia.org/wiki/Generacja_liczb_pseudolosowych)
- [Generowanie liczb losowych w Pythonie - Devcorner](https://devcorner.pl/generowanie-liczb-losowych-w-pythonie/)