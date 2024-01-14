---
title:    "Fish Shell: Formatowanie tekstu do wielkich liter"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z najczęściej wykonywanych operacji podczas pisania kodu jest zmiana stylu tekstowego. Jednym z takich przypadków jest zmiana z małych liter na duże. Pisanie tej samej logiki w różnych miejscach w kodzie jest czasochłonne i może przyczynić się do popełnienia błędów. Dlatego warto poznać narzędzia, które ułatwią nam tę czynność, takie jak funkcja capitalize w Fish Shell.

## Jak to zrobić

Fish Shell oferuje funkcję capitalize, która służy do zmiany pierwszej litery w ciągu znaków na dużą. Aby skorzystać z tej funkcji, należy użyć polecenia:

```Fish Shell
string capitalize string
```

Gdzie "string" to ciąg znaków, którego pierwsza litera ma zostać zamieniona na dużą. Przykładowe użycie można zobaczyć na poniższym przykładzie:

```Fish Shell
echo (capitalize hello world)
```

Output: "Hello world"

Funkcja capitalize działa również z bardziej skomplikowanymi ciągami znaków, na przykład:

```Fish Shell
echo (capitalize "it's a beautiful day")
```

Output: "It's a beautiful day"

W przypadku, gdy pierwszy znak nie jest literą, funkcja nie zwraca żadnego efektu. Na przykład:

```Fish Shell
echo (capitalize "123abc")
```

Output: "123abc"

## Deep Dive

Funkcja capitalize działa poprzez zamianę pierwszego znaku w ciągu znaków na jego wersję z dużej litery. Jest to bardzo przydatne narzędzie przy weryfikacji i poprawianiu danych wprowadzonych przez użytkownika. Jednak należy pamiętać, że zamiana pierwszej litery na dużą może wpływać na wydajność, zwłaszcza w przypadku długich ciągów znaków.

## Zobacz też

- Dokumentacja funkcji capitalize w Fish Shell: https://fishshell.com/docs/current/cmds/capitalize.html
- Poradnik użytkownika Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Inne narzędzia do manipulacji tekstu w Fish Shell: https://fishshell.com/docs/current/tutorial.html#other-commands