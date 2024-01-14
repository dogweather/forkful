---
title:    "Bash: Pisanie testów"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy?

Pisanie testów jest ważnym elementem programowania, ponieważ pozwala na szybkie weryfikowanie poprawności działania naszego kodu. To także pomaga w utrzymaniu wysokiej jakości kodu i ułatwia naprawę błędów. Jest to nieodzowna część procesu tworzenia oprogramowania, zwłaszcza jeśli pracujesz w zespole. W tym artykule przyjrzymy się temu, jak napisać testy w Bash.

## Jak to zrobić?

Bash jest językiem, który jest szeroko stosowany w środowisku Unix i Linux, a więc może być używany do testowania szeregu narzędzi i aplikacji na tych systemach. Najważniejszą częścią pisania testów jest wybranie odpowiedniego narzędzia. W przypadku testów Bash, dobrym wyborem jest narzędzie o nazwie **shellcheck**, które wykrywa błędy w skryptach Bash. Możesz go zainstalować na swoim systemie przy pomocy poleceń `sudo apt-get install shellcheck` dla dystrybucji Ubuntu lub `brew install shellcheck` dla Mac OS.

Po zainstalowaniu narzędzia, pozostaje tylko napisać nasz test. Najprostszy test może wyglądać następująco:

```Bash
# Nasz testowy skrypt Bash
#!/bin/bash
echo "Hello World"
```

Aby uruchomić ten test, możemy użyć polecenia `shellcheck nasz-testowy-skrypt.sh`. Jeśli test przebiegnie pomyślnie, powinniśmy otrzymać taki wynik:

```Bash
In nasz-testowy-skrypt.sh line 2: echo "Hello World"
Convert to: printf "Hello World\n"
```

Oznacza to, że skrypt został przetestowany i nie wykryto w nim żadnych błędów. W przypadku wykrycia jakichkolwiek błędów, narzędzie shellcheck poinformuje nas o nich i poda sugestie naprawy.

## Deep Dive

Pisanie testów w Bash może być nieco trudne, ponieważ ten język nie jest typowo używany do tego celu. Jednak warto poświęcić czas na naukę tego procesu, ponieważ może to zaoszczędzić nam wiele czasu i frustracji w dłuższej perspektywie. Poniżej prezentujemy kilka przydatnych porad dotyczących pisania testów w Bash:

- Pisz krótkie testy: Krótsze testy są łatwiejsze do utrzymania i debugowania, więc warto dzielić je na mniejsze części zamiast tworzyć jeden duży test.

- Testuj konkretną funkcjonalność: Kiedy piszemy testy w Bash, ważne jest, aby skupić się na testowaniu konkretnych funkcjonalności, a nie samego kodu. W ten sposób możemy szybciej wykrywać błędy i zagwarantować poprawne działanie naszego kodu.

- Korzystaj z narzędzi: Oprócz narzędzia shellcheck, istnieje wiele innych przydatnych narzędzi do testowania w Bash, takich jak **shunit2**, **bats** czy **bounshell**. Warto zapoznać się z nimi i wybrać te, które najlepiej odpowiadają naszym potrzebom.

- Używaj asercji: Asercje są bardzo przydatne w testowaniu, ponieważ pozwalają nam sprawdzić oczekiwane wyniki działania naszego kodu. W Bash można skorzystać z opcji `-eq`, `-ne`, `-gt`, `-lt` itp. do porównywania wartości.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o pisaniu testów w Bash, poniżej przedstawiamy kilka wartościowych artykułów:

- [Tworzenie testów jednostkowych w Bash przy u