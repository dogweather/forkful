---
title:    "Ruby: Wydrukowanie danych debugowania"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Podczas pisania kodu w Ruby często dochodzi do sytuacji, kiedy potrzebujemy zrozumieć, co dokładnie dzieje się w naszym programie. W takich przypadkach przydatne jest wypisywanie informacji o zmiennych, krok po kroku, aby móc śledzić działanie programu i znaleźć ewentualne błędy. Właśnie w celu ułatwienia tego procesu, istnieje opcja drukowania tzw. "debug output".

## Jak to zrobić?

Aby wydrukować debug output w Ruby, możemy posłużyć się prostym poleceniem `p` lub `puts`. Poniżej znajdują się przykłady wykorzystania obu poleceń, wraz z ich wynikami w postaci kodu Ruby.

```Ruby
p "To jest przykładowy debug output"
# => "To jest przykładowy debug output"

puts "To jest kolejny przykładowy debug output"
# To jest kolejny przykładowy debug output
# => nil 
```

W przypadku wykorzystania polecenia `p`, widzimy dokładne informacje o wartości i typie zmiennej, natomiast po użyciu `puts` jest to zwykły tekstowy output, bez informacji o typie zmiennej.

## Głębszy wgląd

Warto zauważyć, że oprócz prostej informacji o zmiennej, możemy także wykorzystać tzw. metody `inspect` oraz `to_s`. Dzięki nim, możemy jeszcze bardziej dostosować nasz debug output do własnych potrzeb.

```Ruby
# Przykład wykorzystania metody inspect:
p [1, 2, 3].inspect
# => "[1, 2, 3]"

# Przykład wykorzystania metody to_s:
puts [1, 2, 3].to_s
# [1, 2, 3]
# => nil
```

Drukowanie debug output jest bardzo przydatną techniką, szczególnie podczas pracy z większymi i bardziej skomplikowanymi programami. Dzięki niemu, możemy lepiej zrozumieć działanie naszego kodu i szybko znaleźć ewentualne błędy.

## Zobacz również

Jeśli jesteś zainteresowany/a dalszymi informacjami na temat drukowania debug output w Ruby, polecam zapoznanie się z poniższymi linkami:

- https://www.theodinproject.com/lessons/debugging-your-code
- https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-p
- https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-puts