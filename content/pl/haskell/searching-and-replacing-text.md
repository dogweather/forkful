---
title:    "Haskell: Wyszukiwanie i zamiana tekstu"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Dlaczego?

Niektóre zadania programistyczne mogą być pracochłonne i czasochłonne, takie jak ręczne zamienianie tekstu w pliku. Jednak używając funkcji wyszukiwania i zamiany w Haskellu, możemy to zautomatyzować i zaoszczędzić sobie wiele czasu i wysiłku.

# Jak to zrobić?

```Haskell
main = do
  -- otwarcie pliku z tekstem
  input <- readFile "plik.txt"
  -- zamiana wszystkich wystąpień "urodziny" na "urodzin"
  let output = replace "urodziny" "urodzin" input
  -- zapis nowego tekstu do pliku
  writeFile "nowy_plik.txt" output

replace :: String -> String -> String -> String
replace _ _ [] = []
replace x y z@(c:cs) =
  if x `isPrefixOf` z
    then y ++ replace x y (drop (length x) z) -- zamiana tekstu wewnątrz wyrazu
    else c : replace x y cs                    -- pomijanie reszty tekstu

```

Za pomocą tej prostej funkcji, możemy otworzyć plik z tekstem, zamienić wszystkie wystąpienia wybranej frazy na inną i zapisać nowy plik z zaktualizowanym tekstem. W powyższym przykładzie, zmieniliśmy wszystkie wystąpienia słowa "urodziny" na "urodzin". Jednak funkcja może być dostosowana do dowolnej frazy i zamiany. 

# Deep Dive 

Funkcja `replace` użyta w powyższym przykładzie jest prostym rozwiązaniem dla prostych zadań związanych ze zmianą tekstu. Jednak dla bardziej zaawansowanych zadań, warto zapoznać się z innymi funkcjami, takimi jak `substituteAll` z modułu "Text.Substitute" dostępnego w bibliotece "text". Ta funkcja pozwala na bardziej zaawansowaną kontrolę nad procesem zamiany tekstu poprzez użycie wyrażeń regularnych.

# Zobacz również

- Dokumentacja Haskell: https://www.haskell.org/documentation/
- Biblioteka "text": https://hackage.haskell.org/package/text
- Przykładowe zadania związane ze zmianą tekstu w Haskellu: https://rosettacode.org/wiki/Word_count#Haskell