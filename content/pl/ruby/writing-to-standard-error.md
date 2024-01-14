---
title:    "Ruby: Pisanie do standardowego błędu"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Dlaczego pisać na standardowe wyjście błędów?

Pisanie do standardowego wyjścia błędów jest ważnym narzędziem, które pozwala programistom na monitorowanie i debugowanie swojego kodu. Jest to szczególnie przydatne, gdy tworzymy duże i złożone projekty, gdzie błędy mogą nie tylko utrudnić działanie programu, ale również być przyczyną poważnych problemów dla użytkowników.

## Jak to zrobić?

Aby napisać do standardowego wyjścia błędów w języku Ruby, można skorzystać z metody `warn`, która wypisze podaną wiadomość do wyjścia błędów. W poniższym przykładzie wykorzystamy tę metodę do wyświetlenia komunikatu "Wystąpił błąd!" na wyjściu błędów.

```Ruby
warn "Wystąpił błąd!"
```

Po uruchomieniu programu, powyższy kod spowoduje wyświetlenie podanego komunikatu w konsoli lub terminalu.

### Manipulacja wyjściem błędów

Warto również wiedzieć, że można manipulować wyjściem błędów i kierować go do innych miejsc niż konsola lub terminal. W tym celu można skorzystać z metody `$stderr`, która zwraca strumień wyjściowy dla błędów. Przykładowo, jeśli chcemy zapisać błędy do pliku, możemy wykorzystać poniższy kod:

```Ruby
File.open("errors.txt", "w") do |file|
  $stderr = file
  warn "Błąd został zapisany do pliku."
end
```

Po uruchomieniu programu, błędy zostaną zapisane do pliku "errors.txt", a nie wyświetlone w konsoli lub terminalu.

## Deep Dive

W przypadku, gdy chcemy bardziej szczegółowych informacji o błędach w naszym programie, możemy skorzystać z modułu `Logger` dostępnego w języku Ruby. Jest to narzędzie, które pozwala na logowanie różnego rodzaju informacji, w tym błędów, do pliku lub konsoli z określonym poziomem ważności.

Aby użyć modułu `Logger`, musimy go najpierw zaimportować do naszego programu:

```Ruby
require 'logger'
```

Następnie, możemy utworzyć nowy obiekt loggera i ustawić poziom ważności na `ERROR`, co spowoduje zapisywanie tylko błędów do logów. Przykładowy kod może wyglądać następująco:

```Ruby
logger = Logger.new("logs.txt")
logger.level = Logger::ERROR
```

Wówczas, korzystając z metody `error` obiektu loggera, możemy zapisywać informacje o błędach w naszym programie do pliku "logs.txt".

## Zobacz również

- [Dokumentacja języka Ruby o wypisywaniu do standardowego wyjścia błędów](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-warn)
- [Dokumentacja języka Ruby o manipulowaniu wyjściem błędów](https://ruby-doc.org/core-2.7.1/IO.html#method-c-new-label-I-O+Streams)