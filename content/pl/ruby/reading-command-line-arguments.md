---
title:    "Ruby: Odczytywanie argumentów wiersza poleceń"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak programy mogą odczytać informacje podane z poziomu wiersza poleceń? To właśnie dzięki obsłudze argumentów wiersza poleceń możliwe jest wywoływanie programów z różnymi opcjami i wartościami. W tym artykule dowiesz się, jak w prosty sposób w Ruby odczytać argumenty z wiersza poleceń.

## Jak to zrobić

Aby odczytać argumenty z wiersza poleceń w Ruby, należy skorzystać z metody ARGV. Jest to tablica zawierająca wszystkie argumenty podane przy wywoływaniu programu. Przyjrzyjmy się prostemu przykładowi kodu:

```Ruby
ARGV.each do |arg|
  puts arg
end
```

Ten kod wypisze na ekranie wszystkie argumenty podane przy uruchomieniu programu. Jeśli na przykład nasz program nazywa się "test.rb" i uruchomiliśmy go z argumentami "hello world", otrzymamy na wyjściu:

```
hello
world
```

Zauważ, że wielkość liter jest zachowana, więc jeśli chcemy, aby nasze programy odczytywały argumenty niezależnie od ich wielkości, należy odpowiednio to przewidzieć w kodzie.

Możemy również przypisać każdy argument do zmiennej, aby móc nimi manipulować. Przykładowo:

```Ruby
first_arg = ARGV[0]
puts first_arg
```

Ten kod wypisze tylko pierwszy argument, czyli "hello".

## Głębszy zanurzanie się w temat

Warto wspomnieć, że ARGV traktuje każde słowo oddzielone spacjami jako oddzielny argument. Jeśli chcemy przyjąć jako argument całe zdanie, musimy zawrzeć je w cudzysłowie. Na przykład:

```Ruby
`puts ARGV[0]
```

Jeśli uruchomimy ten kod z argumentami "hello world", otrzymamy na wyjściu "hello", ale jeśli uruchomimy z argumentem "hello world" (bez cudzysłowów), otrzymamy na wyjściu tylko "hello", a drugi argument zostanie zignorowany.

Spróbuj również poeksperymentować z różnymi opcjami wiersza poleceń, na przykład "-i" lub "--version", aby lepiej zrozumieć jak ARGV działa.

## Zobacz także

- [Dokumentacja Ruby o ARGV](https://ruby-doc.org/core-2.7.0/ARGV.html)
- [Przykładowy kod odczytujący argumenty z wiersza poleceń w Ruby](https://gist.github.com/cadecairos/8554681)
- [Inne możliwości manipulacji argumentami wiersza poleceń w Ruby](https://medium.com/rubycademy/from-the-command-line-to-a-ruby-program-e7a2e13604a3)