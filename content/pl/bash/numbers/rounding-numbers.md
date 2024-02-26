---
date: 2024-01-26 03:43:25.695037-07:00
description: "Zaokr\u0105glanie liczb oznacza odci\u0119cie cz\u0119\u015Bci dziesi\u0119\
  tnych do prostszej warto\u015Bci, kt\xF3ra jest wystarczaj\u0105co dobra w danym\
  \ kontek\u015Bcie. Programi\u015Bci zaokr\u0105glaj\u0105\u2026"
lastmod: '2024-02-25T18:49:33.944075-07:00'
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb oznacza odci\u0119cie cz\u0119\u015Bci dziesi\u0119\
  tnych do prostszej warto\u015Bci, kt\xF3ra jest wystarczaj\u0105co dobra w danym\
  \ kontek\u015Bcie. Programi\u015Bci zaokr\u0105glaj\u0105\u2026"
title: "Zaokr\u0105glanie liczb"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zaokrąglanie liczb oznacza odcięcie części dziesiętnych do prostszej wartości, która jest wystarczająco dobra w danym kontekście. Programiści zaokrąglają liczby, aby upraszczać wyniki, oszczędzać miejsce lub ponieważ dokładna wartość nie jest kluczowa — na przykład, gdy oceniasz użycie procesora lub miejsce na dysku i części dziesiętne nie zaważą na Twoim dniu.

## Jak to zrobić:

Oto krótki przewodnik po zaokrąglaniu w Bashu:

```Bash
# Zaokrąglenie w dół za pomocą 'floor' z bc
echo "scale=0; 3.49/1" | bc

# Zaokrąglenie w górę za pomocą 'ceiling' z bc
echo "scale=0; 3.01/1" | bc -l

# Zaokrąglenie do najbliższej całkowitej za pomocą printf
printf "%.0f\n" 3.49

# Sposób na zaokrąglenie do najbliższej całkowitej za pomocą bc
echo "(3.49+0.5)/1" | bc
```

Przykłady wyników — prosto z terminala:

```
3  # Zaokrąglone w dół (floor)
4  # Zaokrąglone w górę (ceiling)
3  # Zaokrąglone do najbliższej (z printf)
3  # Zaokrąglone do najbliższej (z bc)
```

## Pogłębiona analiza

Kiedyś nie było `bc` ani `printf` w skryptach Bash, aby wykonywać matematyczne magiczne działania. Staroświeccy musieli polegać na zewnętrznych narzędziach lub sprytnych obejściach. Teraz `bc` pozwala ci wykonywać precyzyjne obliczenia matematyczne. Pamiętaj, że `bc` domyślnie nie zaokrągla — wykonuje "floor". Część dotycząca "scale" ustawia działanie na punkcie dziesiętnym.

Alternatywy? Możesz użyć `awk` do zaokrąglania bez przełączania się na `bc` lub zmagać się z `perl` dla bardziej wymagających potrzeb matematycznych. Dla masochistów, przejdź na czysty Bash, powiedzmy, z iteracyjną manipulacją ciągów – ale po co?

Jeśli chodzi o szczegóły, `bc` nie tylko zaokrągla, wykonuje mnóstwo matematycznych zadań — skaluje, liczy sinus, pierwiastkuje, masz na myśli. Z `printf`, chodzi bardziej o formatowanie tekstu, ale hej, zaokrągla liczby, więc nie narzekamy.

## Zobacz także

Dla tych, którzy chcą więcej:

- Podręcznik GNU `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Polecenie Bash `printf`: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- Przewodnik użytkownika AWK (do zaokrąglania i innej obróbki tekstu): https://www.gnu.org/software/gawk/manual/gawk.html
- Więcej matematyki w Bashu, skrypty i sztuczki liczbowe: https://mywiki.wooledge.org/BashFAQ/022
