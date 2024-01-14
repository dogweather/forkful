---
title:    "Bash: Konwertowanie tekstu na małe litery"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ludzie decydują się na konwersję ciągu znaków na małe litery w programowaniu? Istnieje wiele powodów, dlaczego taka operacja może być potrzebna. Może chcesz porównać dwa ciągi znaków bez uwzględniania wielkości liter, lub może chcesz upewnić się, że wszystkie wprowadzone dane są w jednym formacie, niezależnie od tego, jak użytkownik je wprowadził. Bez względu na powód, przejście na małe litery jest przydatne i łatwe do zrobienia w języku Bash.

## Jak to zrobić

Aby przekonwertować ciąg znaków na małe litery w Bash, możemy użyć wbudowanej funkcji `tr`, która służy do przetwarzania i filtrowania tekstu. Użyjemy jej w połączeniu z opcją `-s`, która usuwa powtarzające się znaki i zamienia je na jeden znak. W ten sposób zachowujemy tylko znaki, które chcemy przekonwertować. Następnie wykorzystamy opcję `[:upper:]` i `[:lower:]`, które określają duże i małe litery. Na koniec podajemy nasz ciąg znaków jako argument i zwracamy wynik.

```Bash
echo "PRzykład TekSTU" | tr -s '[:upper:]' '[:lower:]'
```

Przykład ten zwróci "przykład tekstu" jako wynik. Możemy także przypisać wynik do zmiennej i wykorzystać go w naszym kodzie. 

```Bash
input="PRzykład TekSTU"
output=$(echo $input | tr -s '[:upper:]' '[:lower:]')
echo $output
```

W tym przypadku otrzymamy ten sam wynik, ale możemy łatwo wykorzystać go w innych częściach naszego skryptu. Pamiętaj, że ta metoda nie będzie działać z polskimi znakami diakrytycznymi, więc jeśli chcesz zachować duże litery z polskimi znakami, możesz wykorzystać opcję `tr -d '[:upper:]'`.

## Głębszy wgląd

Funkcja `tr` jest bardzo użyteczną i wszechstronną funkcją w Bashu, ale może zająć trochę czasu, aby oswoić się z jej różnymi opcjami. Przekonwertowanie ciągu znaków na małe litery przydatne jest również, gdy chcemy przeszukiwać duże ilości danych, ponieważ pomaga nam w filtrowaniu i sortowaniu tekstów. Ponadto, jeśli chcesz poznać więcej przydatnych opcji funkcji `tr`, zachęcam do zbadania dokumentacji lub innych artykułów na ten temat.

## Zobacz także

- [Oficjalna dokumentacja funkcji `tr`](https://www.gnu.org/software/sed/manual/html_node/The-_0022tr_0022-Command.html)
- [Przykładowe użycie funkcji `tr` w praktyce](https://www.linuxjournal.com/article/3764)