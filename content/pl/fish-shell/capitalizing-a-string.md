---
title:                "Fish Shell: Zapisywanie tekstu jako wielkie litery"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać Fish Shell do zapisywania ciągów

Jeśli jesteś programistą lub hobbystą, który lubi pracować z językiem programowania, z pewnością spotkałeś się z sytuacją, w której musiałeś zmienić wielkość liter w tekście. Jest to codzienne wyzwanie, szczególnie jeśli pracujesz z dużymi ilościami danych. Dlatego dzisiaj przyjrzymy się funkcji z Fish Shell, która ułatwia operacje na tekstach - zmiana wielkości liter. 

## Jak to zrobić?

Python jest językiem programowania, który oferuje wiele wbudowanych funkcji, które ułatwiają pracę z tekstami. Jedną z tych funkcji jest metoda `capitalize()`, która zmienia pierwszą literę w tekście na dużą, a wszystkie pozostałe na małe. Jest to bardzo przydatne narzędzie do standardyzacji danych lub tworzenia estetycznych wyjść.

```Fish Shell
set nazwa = "fish shell to super!"
echo $nazwa
# wyjście: fish shell to super!
echo $nazwa | capitalize
# wyjście: Fish shell to super!
```

W powyższym przykładzie, dzięki zastosowaniu metody `capitalize()`, tekst zaczyna się od wielkiej litery, a reszta jest zapisana małymi literami. Warto pamiętać, że ta metoda nie tylko zmienia wielkość liter, ale również zachowuje odstępy między wyrazami.

Za pomocą Fish Shell można również zmienić tylko pierwszą literę w tekście na dużą, pozostawiając pozostałe bez zmian. W tym celu użyjemy funkcji `string replace`, która zamienia wybrany fragment tekstu na inny.

```Fish Shell
set nazwa = "FISH shell jest niesamowitym narzędziem!"
echo $nazwa
# wyjście: FISH shell jest niesamowitym narzędziem!
string replace upper $nazwa 1 1 $nazwa
echo $nazwa
# wyjście: Fish shell jest niesamowitym narzędziem!
```

W powyższym przykładzie, za pomocą funkcji `string replace`, zamieniamy pierwszą literę tekstu na dużą, pozostawiając resztę tekstu bez zmian.

## Dogłębna analiza

Mimo że zmiana wielkości liter w tekście wydaje się prosta, może być czasochłonna, szczególnie w przypadku dużych danych. Dlatego warto skorzystać z narzędzi, które ułatwią nam pracę. Metoda `capitalize()` to tylko jedna z wielu dostępnych w Fish Shell, a korzystając z innych funkcji, możemy dostosować wielkość liter w tekście do naszych potrzeb.

Na przykład, jeśli chcemy zmienić wszystkie litery w tekście na duże, możemy skorzystać z metody `upper()`. Natomiast jeśli chcemy zamienić wszystkie litery na małe, wystarczy użyć funkcji `lower()`. Ponadto, przy operacjach na dłuższych ciągach tekstowych, warto zwrócić uwagę na wydajność funkcji i wybierać te najbardziej optymalne.

# Zobacz też

* Dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
* Szybki start z Fish Shell: https://github.com/jorgebucaran/fish-shell-setup
* Poradnik Fish Shell dla początkujących: https://medium.com/@thechampl8/setting-up-the-perfect-fish-shell-3ee7db3cecde