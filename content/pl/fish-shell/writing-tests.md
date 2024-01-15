---
title:                "Pisanie testów"
html_title:           "Fish Shell: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego piszesz testy?

Jeśli jesteś programistą, na pewno zetknąłeś się z procesem pisania testów. Czy to na wyższym poziomie, korzystając z narzędzi do testowania, czy dopiero zaczynając swoją przygodę w świecie kodu, testowanie jest nieodłącznym elementem. Dlaczego więc warto poświęcić czas na pisanie testów?

Po pierwsze, testy pozwalają nam na wykrywanie błędów w naszym kodzie wcześniej. To oznacza, że jesteśmy w stanie znaleźć i naprawić problemy zanim zostaną wypuszczone do produkcji. Dzięki temu unikamy niepotrzebnych awarii i oszczędzamy czas oraz pieniądze.

Po drugie, testy dają nam pewność, że nasz kod działa poprawnie. Możemy mieć pewność, że nasze zmiany nie wpłynęły negatywnie na już istniejący kod. To pozwala nam na szybsze wprowadzanie nowych funkcji i aktualizacji.

## Jak pisać testy w Fish Shell?

### Przygotowanie

Aby zacząć pisać testy w Fish Shell, potrzebujemy narzędzia o nazwie "fish_assert". Jest ono dostępne w większości dystrybucji Fish Shell oraz można je pobrać z Githuba.

### Przykład

Załóżmy, że chcemy napisać test sprawdzający, czy podana liczba jest dodatnia. W tym celu użyjemy poniższego kodu:

```
Fish Shell
function is_positive
    set -l number $argv[1]
    if math "$number > 0"
        return 0
    else
        return 1
    end
end

source ../assert.fish

fish_assert "is_positive 5" true 
```

W powyższym przykładzie definiujemy funkcję "is_positive", która przyjmuje jedną liczbę jako argument i zwraca wartość "true", jeśli jest ona większa od zera. Następnie, używamy narzędzia "fish_assert", aby sprawdzić, czy dla liczby 5 funkcja zwróci prawidłową wartość. 

### Wynik

Po uruchomieniu powyższego kodu, otrzymamy następujący output:

```
Fish Shell
Assertion Failed: is_positive 5 equals true
```

Jest to informacja, że funkcja "is_positive" przy danych parametrach zwróciła nieprawidłową wartość. W ten sposób możemy w łatwy sposób znaleźć błędy w naszym kodzie.

## Dogłębne wyjaśnienie

Pisanie testów jest ważnym elementem w tworzeniu wysokiej jakości kodu. Nie tylko pomaga nam wykrywać błędy i utrzymywać pewność co do poprawności naszego kodu, ale również ułatwia przyszłe zmiany i aktualizacje. 

Istnieją różne typy testów, takie jak jednostkowe, integracyjne czy end-to-end. W Fish Shell najczęściej wykorzystywane są testy jednostkowe, które sprawdzają pojedyncze elementy kodu. Dobrym zwyczajem jest także pisanie testów przed napisaniem kodu, ponieważ pozwala to na zdefiniowanie oczekiwanych wyników oraz ułatwia projektowanie kodu.

Podczas pisania testów ważne jest, aby nie pisać nadmiernie skomplikowanych testów, które mogą być trudne do zrozumienia i utrzymania w przyszłości. Ważne jest również, aby testować różne przypadki w celu znalezienia potencjalnych błędów.

## Zobacz także

- [Official Fish Shell Website](https://fishshell.com/)
- [GitHub repository for Fish Assert](https://github.com/fish-shell/fish-assert)
- [Tutorial on Writing Unit Tests in Fish