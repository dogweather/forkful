---
title:                "Clojure: Usuwanie znaków odpowiadających wzorcowi"
simple_title:         "Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas programowania w Clojure zdarza się, że musimy usunąć wszystkie znaki w ciągu znaków, które pasują do określonego wzorca. Może to być potrzebne, gdy musimy oczyścić dane lub przeprowadzić inne operacje na tekście. W tym wpisie opowiemy o tym, jak można to zrobić za pomocą kawałków kodu w Clojure.

## Jak to zrobić

Jednym z prostych sposobów na usunięcie wszystkich znaków pasujących do wzorca jest użycie funkcji `replace` wraz z wyrażeniem regularnym. Przykładowo, jeśli chcemy usunąć wszystkie cyfry z ciągu znaków, możemy wykorzystać następujący kod:

```Clojure
(def tekst "abc123def456ghi")
(replace #"\d" "" tekst)
```

W powyższym przykładzie, tekst zostanie przeparsowany przez wyrażenie regularne `#"\d"`, które oznacza "znajdź wszystkie cyfry". Następnie, te znaki zostaną zastąpione przez pusty ciąg znaków. W rezultacie, otrzymamy nowy ciąg bez cyfr: `abcdefghi`.

Możemy również użyć funkcji `filter` wraz z wyrażeniem regularnym, aby usunąć wszystkie znaki pasujące do wzorca z danego ciągu. Przykładowo, jeśli chcemy usunąć wszystkie samogłoski z tekstu, możemy wykorzystać ten kod:

```Clojure
(def tekst "programowanie")
(apply str (filter #"(?i)[aeiouy]" tekst))
```

W tym przypadku, wyrażenie regularne `#"(?i)[aeiouy]"` oznacza "znajdź wszystkie samogłoski bez uwzględniania wielkości liter". Następnie, za pomocą funkcji `apply` i `str` wypełniamy pustym ciągiem każdy znak, który pasuje do wzorca, uzyskując w ten sposób ciąg bez samogłosek: `prgrmwn`.

Możliwości jest wiele, a wybór metody zależy od naszego konkretnego przypadku. Warto jednak pamiętać, że wyrażenia regularne wraz z odpowiednimi funkcjami w Clojure są bardzo przydatne w usuwaniu znaków pasujących do wzorca.

## Deep Dive 

W Clojure, wyrażeniem regularnym jest ciąg znaków zwracany przez funkcję `re-pattern`. Na przykład, wyrażenie `#"[a-z]+"` zostanie przekonwertowane na wyrażenie regularne, które będzie pasować do wszystkich ciągów znaków zawierających małe litery od a do z.

Ważne jest także, aby pamiętać o wyrażeniach regularnych wielu wyrazów, które mogą być użyteczne w bardziej skomplikowanych przypadkach. Na przykład, wyrażenie `#"(?i)[aeiouy]+` będzie pasować do wszystkich samogłosek, niezależnie od ich wielkości liter.

Łącząc te informacje z funkcjami `replace` i `filter`, możemy dokonywać bardzo precyzyjnych operacji na tekście i usuwać tylko te znaki, których chcemy się pozbyć.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych i funkcjach w Clojure, zapoznaj się z poniższymi linkami:

- [Dokumentacja Clojure](https://clojure.org/documentation)
- [Oficjalny tutorial Clojure](https://clojure.org/guides/getting_started)
- [Książka "Clojure for the Brave and True" autorstwa Danieli Higginbotham](https://www.braveclojure.com/clojure-for-the-brave-and-true/)