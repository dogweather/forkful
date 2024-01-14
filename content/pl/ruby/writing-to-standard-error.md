---
title:    "Ruby: Pisanie do standardowego błędu"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu (standard error) może wydawać się frustrujące i niepotrzebne, ale jest to bardzo ważne narzędzie dla programistów Ruby. Dzięki temu, możemy monitorować, debugować i poprawiać nasze programy. W tym artykule dowiesz się, dlaczego pisanie do standard error jest ważne i jak to zrobić.

## Jak To Zrobić

Aby pisać do standardowego błędu w Ruby, musimy użyć metody `STDERR.puts`. Spójrzmy na przykład:

```ruby
10 / 0
```

Napisanie tego kodu spowoduje błąd dzielenia przez 0, ale nie zostanie wyświetlony żaden komunikat w konsoli. Jednak jeśli użyjemy metody `STDERR.puts`, wyświetli się ona w standardowym błędzie, który jest wyświetlany w konsoli na czerwono:

```ruby
STDERR.puts "Nie można dzielić przez 0!"
# => Nie można dzielić przez 0!
```

Możemy również użyć tej metody do wyświetlania innych informacji o błędzie, np. numeru linii, w którym wystąpił błąd:

```ruby
STDERR.puts "Wystąpił błąd w linii #{__LINE__}."
# => Wystąpił błąd w linii 3.
```

## Deep Dive

W Ruby, `puts` i `print` są metodami do wypisywania tekstu na standardowym wyjściu (standard output). Natomiast `STDERR.puts` służy do wypisywania na standardowym błędzie. W przypadku błędów, takich jak wyjątki lub nieoczekiwane zachowania programu, wypisanie informacji na standardowym błędzie jest bardziej odpowiednie, ponieważ pomaga nam zlokalizować i naprawić problem.

Należy również pamiętać, że jeśli nie użyjemy metody `STDERR.puts`, błędy i wyjątki będą nadal wyświetlane, ale tylko na standardowym wyjściu. W takim przypadku będą wyświetlane na białym tle, co może być trudne do zauważenia, zwłaszcza jeśli mamy dużo tekstu w konsoli.

## Zobacz też

- [Dokumentacja Ruby: standard error](https://ruby-doc.org/core-2.6.3/StandardError.html)
- [Inne sposoby obsługi błędów w Rubym](https://www.rubyguides.com/2019/04/ruby-error-handling/)
- [Poradnik dla początkujących w tworzeniu aplikacji w Ruby](https://dev.to/t/beginners/ruby)