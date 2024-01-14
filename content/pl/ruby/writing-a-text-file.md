---
title:    "Ruby: Tworzenie pliku tekstowego"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego

Dlaczego warto tworzyć pliki tekstowe w języku Ruby?

Dzięki plikom tekstowym możemy przechowywać dane w formacie łatwym do czytania i modyfikacji przez komputer. Jest to szczególnie przydatne w przypadku przechowywania dużej ilości informacji, takich jak bazy danych lub konfiguracje.

# Jak to zrobić

Aby utworzyć plik tekstowy w języku Ruby, wystarczy użyć funkcji "File.new". Przykładowy kod możesz znaleźć poniżej:

```Ruby
plik = File.new("tekstowy_plik.txt", "w+")
plik.puts "Przykładowy tekst"
plik.close
```

Powyższy kod utworzy plik o nazwie "tekstowy_plik.txt" i zapisze w nim napis "Przykładowy tekst". Zwróć uwagę na drugi argument funkcji "w+", który oznacza, że chcemy nadpisać zawartość pliku, jeśli już istnieje.

Możesz również wykorzystać funkcję "File.open" do otwarcia istniejącego pliku i dopisywania do niego zawartości. Przykład:

```Ruby
plik = File.open("tekstowy_plik.txt", "a")
plik.puts "Kolejny tekst"
plik.close
```

# Wprowadzenie w teorię

Istnieje kilka ważnych pojęć związanych z tworzeniem plików tekstowych w języku Ruby, które warto zrozumieć.

### Pola tekstowe i znaki kontrolne

Początkowo plik tekstowy może wydawać się po prostu zbiorowiskiem liter i cyfr, ale istnieją również tzw. "znaki kontrolne". Są to specjalne znaki, które wykorzystywane są do wywoływania określonych akcji, takich jak zmiana linii czy wcięcie tekstu.

### Kodowanie znaków

Ważnym aspektem plików tekstowych jest kodowanie znaków. Język Ruby domyślnie stosuje kodowanie UTF-8, które obsługuje większość znaków używanych w różnych językach. Jednak w przypadku pracy z plikami tekstowymi w innym kodowaniu, należy zwrócić uwagę na odpowiednie deklaracje w kodzie.

# Zobacz także

- Dokumentacja języka Ruby - [https://ruby-doc.org/](https://ruby-doc.org/)
- Kurs Ruby w Codecademy (w języku polskim) - [https://www.codecademy.com/learn/learn-ruby](https://www.codecademy.com/learn/learn-ruby)
- Historia języka Ruby - [https://en.wikipedia.org/wiki/Ruby_(programming_language)#History](https://en.wikipedia.org/wiki/Ruby_(programming_language)#History)