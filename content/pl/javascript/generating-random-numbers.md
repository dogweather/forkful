---
title:    "Javascript: Generowanie losowych liczb"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią wielu programów i aplikacji. Może ono być wykorzystane do symulowania przypadkowych zdarzeń, generowania unikalnych identyfikatorów czy też do tworzenia gier. W tym artykule dowiesz się, jak w łatwy sposób generować losowe liczby w języku Javascript.

## Jak to zrobić

Do wygenerowania losowej liczby w Javascript możemy wykorzystać metodę `Math.random()`, która zwraca wartość z przedziału od 0 do 1. Jednakże, chcąc uzyskać liczbę w konkretnym przedziale, musimy wykonać kilka dodatkowych kroków. Przykładowo, jeśli chcemy wygenerować liczbę całkowitą z przedziału od 1 do 10, możemy skorzystać z poniższego kodu:

```Javascript
// Funkcja generująca losową liczbę całkowitą z przedziału od 1 do 10
function randomInRange() {
  return Math.floor(Math.random() * 10) + 1;
}

// Przykładowe wywołanie funkcji
console.log(randomInRange()); // wypisze np. 5, 9, 2, itp. 
```

W powyższym przykładzie, funkcja `randomInRange()` wykorzystuje metodę `Math.floor()` do zaokrąglenia wygenerowanej liczby w dół oraz dodaje 1, aby uzyskać liczbę z przedziału od 1 do 10. Podobnie, możemy wykorzystać metodę `Math.ceil()` do zaokrąglenia w górę lub metodę `Math.round()` do zaokrąglenia do najbliższej liczby całkowitej.

Możliwości jest wiele, np. możemy generować losowe liczby zmiennoprzecinkowe, losowe znaki czy też wykorzystać argumenty typu `min` i `max` dla większej elastyczności. Warto również pamiętać, że każde wywołanie funkcji `Math.random()` będzie zwracać inną wartość.

## Głębszy wgląd

Istnieje wiele algorytmów generowania liczb losowych, a wybranie odpowiedniego zależy od naszych potrzeb. Jednym z powszechnych sposobów jest wykorzystanie wyniku poprzedniego wywołania funkcji `Math.random()` jako naszego "ziarna" (ang. seed). Możemy również wykorzystać biblioteki i narzędzia, takie jak lodash, do generowania bardziej złożonych lub unikalnych liczb.

Dobrze zaprojektowana funkcja generująca losowe liczby musi również uwzględniać szybkość i bezpieczeństwo. W przypadku aplikacji, które wymagają zapewnienia losowości w celach kryptograficznych, należy skorzystać z dedykowanych bibliotek lub wbudowanych w język funkcji.

## Zobacz też

- [Dokumentacja Math.random() w języku Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Math/random)
- [Biblioteka lodash](https://lodash.com/docs/4.17.15#random)
- [Generator liczb losowych w kryptografii](https://pl.wikipedia.org/wiki/Generator_liczb_losowych#Kryptograficzne_generatory_liczb_losowych)