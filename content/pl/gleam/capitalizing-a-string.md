---
title:    "Gleam: Zmiana wielkości liter w ciągu znaków"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Dlaczego Warto Dużyć Ciąg Tekstu w Języku Gleam?

Dużyć ciąg tekstu jest powszechnie stosowaną czynnością w wielu programach, szczególnie jeśli pracujemy z danymi wejściowymi podanymi przez użytkownika lub ze zmiennymi przechowywanymi w kodzie. W tym blogu opowiemy o tym, jak wykorzystać język Gleam do dużyenia ciągów tekstowych oraz jakie korzyści to przyniesie.

## Jak Dużyć Ciąg Tekstu w Języku Gleam?

Działanie dużyenia ciągu tekstu jest bardzo proste - polega ono na zamianie pierwszej litery w danym wyrazie na dużą. W języku Gleam można to osiągnąć za pomocą funkcji `String.capitalize()`. Spróbujmy to zobrazować na przykładowym kodzie:

```Gleam
let name = "marek"
let name_capitalized = String.capitalize(name)

// Wynik: "Marek"
```

W powyższym przykładzie zmienna `name_capitalized` będzie przechowywać wartość "Marek", ponieważ funkcja `String.capitalize()` zmieniła pierwszą literę w wyrazie "marek" na dużą.

Jeśli chcemy dużyć nie tylko pierwszą literę, ale wszystkie litery w ciągu tekstu, można skorzystać z funkcji `String.capitalize_all()`. Poniższy przykład pokaże, jak to działa:

```Gleam
let sentence = "czesc wszystkim"
let sentence_capitalized = String.capitalize_all(sentence)

// Wynik: "Czesc Wszystkim"
```

Warto również zauważyć, że można dużyć nie tylko pojedyncze wyrazy, ale całe zdania bądź nawet akapity tekstu.

## Zagłębienie w Funkcjonalność Dużyenia Ciągu Tekstu

Dużyenie ciągów tekstowych jest tylko jedną z wielu przydatnych funkcji, które oferuje język Gleam. Pozwala on na wygodną i szybką manipulację tekstami w naszych programach. Warto pamiętać, że język Gleam jest statycznie typowany, co oznacza, że zmienne przekazane do funkcji dużyenia muszą być typu `String`.

Innym istotnym aspektem jest to, że funkcje dużyenia są bardzo wydajne. Oznacza to, że mogą być wykorzystywane w programach, w których wydajność jest kluczowa, np. w aplikacjach internetowych czy grach.

# Zobacz również

- Dokumentacja języka Gleam na stronie https://gleam.run/
- Przykładowe projekty wykorzystujące funkcję dużyenia ciągów tekstu w języku Gleam: https://github.com/search?l=Gleam&q=string+capitalize&type=Code