---
title:                "Fish Shell: Drukowanie wyjścia do debugowania"
simple_title:         "Drukowanie wyjścia do debugowania"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą i używasz Fish Shell do pisania skryptów lub automatyzacji zadań, prawdopodobnie spotkałeś się z potrzebą debugowania swojego kodu. W takich sytuacjach niezwykle przydatne jest wypisywanie informacji, które pomagają zrozumieć działanie programu i znajdować błędy. W tym wpisie dowiesz się, jak wykorzystać funkcję drukowania debug output w Fish Shell.

## Jak to zrobić

Drukowanie debug output w Fish Shell jest bardzo proste. Wystarczy użyć wbudowanej funkcji `echo` i przekazać jej wiadomość lub zmienną, którą chcesz wyświetlić. Oto przykładowy kod:

```Fish Shell
set my_var "Hello World"
echo $my_var
```

W powyższym przykładzie, do zmiennej `my_var` przypisujemy tekst "Hello World", a następnie wyświetlamy go na ekranie za pomocą funkcji `echo`. W wyniku otrzymamy na konsoli napis "Hello World". Proste, prawda?

Możesz również wyświetlać wiadomości bezpośrednio wewnątrz kodu, aby uzyskać dodatkowe informacje na temat działania programu. Na przykład:

```Fish Shell
mkdir test_folder
echo "Utworzyłem folder testowy"
```

Dzięki temu będziemy wiedzieli, czy operacja utworzenia folderu przebiegła pomyślnie.

## Deep Dive

Jeśli chcesz wypisać więcej informacji niż tylko jedną wiadomość lub zmienną, możesz użyć funkcji `printf` w połączeniu z formatowaniem. Oto przykładowy kod:

```Fish Shell
set my_var "Hello"
set another_var "World"
printf "%s %s!" $my_var $another_var
```

W wyniku otrzymamy tekst "Hello World!". Zauważ, że w funkcji `printf` wstawiamy zmienne za pomocą symbolu `$` oraz określamy formatowanie wiadomości. Możesz dowolnie zmieniać formatowanie, wykorzystując specjalne symbole takie jak `%s` (dla stringów) czy `%d` (dla liczb całkowitych).

Dzięki wykorzystaniu funkcji `echo` i `printf`, możesz wypisać dowolne informacje potrzebne w procesie debugowania, asystując sobie w szybkim znajdowaniu błędów czy zrozumieniu działania programu.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o funkcjach Fish Shell, zapoznaj się z poniższymi linkami:

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Przydatne funkcje w Fish Shell](https://www.techrepublic.com/article/learn-this-overlooked-shell-for-apple-users/)