---
date: 2024-01-26 00:36:59.711946-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w w skryptach Bash polega na przewidywaniu,\
  \ gdzie mog\u0105 pojawi\u0107 si\u0119 problemy i radzenie sobie z nimi w elegancki\
  \ spos\xF3b. Dlaczego? Poniewa\u017C\u2026"
lastmod: '2024-03-11T00:14:08.783650-06:00'
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w w skryptach Bash polega na przewidywaniu,\
  \ gdzie mog\u0105 pojawi\u0107 si\u0119 problemy i radzenie sobie z nimi w elegancki\
  \ spos\xF3b. Dlaczego? Poniewa\u017C\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obsługa błędów w skryptach Bash polega na przewidywaniu, gdzie mogą pojawić się problemy i radzenie sobie z nimi w elegancki sposób. Dlaczego? Ponieważ zapewnia to niezawodność skryptu i oszczędza użytkownikom drapania się po głowie, kiedy rzeczy nie działają tak, jak się tego oczekuje.

## Jak to zrobić:

```Bash
#!/bin/bash

# Przekierowanie stderr do pliku
grep "something" file.txt 2> errors.log

# Obsługa błędów za pomocą statusów wyjścia
if ! grep "something" file.txt; then
    echo "Ups, coś poszło nie tak podczas szukania 'something'."
    exit 1
fi

# Użycie trap do sprzątania przed opuszczeniem na skutek błędu
cleanup() {
  echo "Czyszczenie plików tymczasowych..."
  rm temp_*
}

trap cleanup ERR

# celowy błąd: plik nie istnieje
cat temp_file.txt
```

Przykładowe wyjście, gdy wystąpi błąd:

```
Czyszczenie plików tymczasowych...
cat: temp_file.txt: Nie ma takiego pliku ani katalogu
```

## Szczegółowa analiza

Obsługa błędów w skryptach Bash sięga początków powłoki Unix, gdzie niezawodne i solidne skrypty były (i są) istotne dla administracji systemem i automatyzacji. Tradycyjnie błędy w Bashu są obsługiwane poprzez sprawdzanie statusu wyjścia komendy, która przez konwencję zwraca 0 w przypadku sukcesu i wartość niezerową w przypadku porażki.

Bash wprowadził polecenie `trap` jako wbudowaną funkcję, pozwalając użytkownikom określać komendy wykonywane na różnych sygnałach lub przy wyjściu ze skryptu. Jest to przydatne do zadań porządkowych lub jako ostateczny mechanizm obsługi błędów.

Istnieje również polecenie `set`, które może zmienić zachowanie Basha podczas błędów. Na przykład `set -e` spowoduje, że skrypt natychmiast zakończy działanie, jeśli jakakolwiek komenda zakończy się ze statusem niezerowym, jako sposób na szybkie niepowodzenie i uniknięcie łańcuchowych błędów.

Alternatywy dla wbudowanej obsługi błędów Basha obejmują jawne sprawdzanie istnienia plików, używanie zastąpienia polecenia lub nawet pisanie własnych funkcji do bardziej szczegółowej obsługi błędów.

Chociaż rygorystyczna obsługa błędów może czasem wydawać się przesadną dla małych skryptów, jest to praktyka, która może zaoszczędzić dużo czasu na debugowaniu i zapobiegać nieoczekiwanemu zachowaniu zarówno dla Ciebie, jak i użytkowników.

## Zobacz również

- Podręcznik Basha o parametrach powłoki: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Sekcja o obsłudze błędów w Przewodniku Zaawansowanego Skryptowania w Bashu: https://www.tldp.org/LDP/abs/html/exit-status.html
- Wyczerpujący przewodnik na temat `trap`: https://mywiki.wooledge.org/SignalTrap

Pamiętaj, skryptowanie to forma sztuki i to, jak radzisz sobie z potknięciami i upadkami, może uczynić twoje dzieło bardziej wytrzymałym. Miłego skryptowania!
