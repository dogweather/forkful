---
title:    "Go: Wydobywanie podciągów"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego używać funkcji wyciągających podciągi w Go?

Wyobraź sobie, że masz długi ciąg znaków i musisz wyodrębnić z niego tylko pewną część, która jest ważna dla Twojego kodu. W takim przypadku funkcje wyciągające podciągi są niezwykle przydatne i ułatwiają pracę programistom.

# Jak używać funkcji wyciągających podciągi w Go?

Aby wyodrębnić podciąg w Go, musisz użyć wbudowanej funkcji `Substring`. Przykładowy kod wyglądałby następująco:

```
ciag_znakow := "To jest przykładowy ciąg znaków."
podciag := Substring(ciąg_znaków, 8, 17)
fmt.Println(podciag)
```

W powyższym przykładzie funkcja `Substring` wyodrębnia podciąg z `ciag_znakow` zaczynając od 8 znaku i kończąc na 17. Jeśli chcesz wyodrębnić cały ciąg od danego znaku do końca, możesz pominąć trzeci argument funkcji.

# Głębsza analiza funkcji wyciągających podciągi

Funkcja `Substring` przyjmuje trzy argumenty - pierwszy to ciąg znaków, z którego chcesz wyodrębnić podciąg, drugi to indeks początkowy, od którego chcesz zacząć wyodrębnianie, a trzeci to opcjonalny indeks końcowy. Jeśli podasz tylko dwa argumenty, funkcja automatycznie zacznie wyodrębnianie od podanego indeksu do końca ciągu.

Ważną rzeczą do zapamiętania jest to, że indeksowanie w Go zaczyna się od 0, czyli pierwszy znak ma indeks 0, drugi znak ma indeks 1 itd. Dzięki temu możesz precyzyjnie wybrać żądany podciąg.

# Zobacz także

- Dokumentacja Go dla funkcji `Substring`: https://golang.org/pkg/strings/#Substring
- Przewodnik po podstawowych funkcjach w Go: https://tour.golang.org/basics/11