---
date: 2024-01-26 00:52:33.841649-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w pozwala skryptowi radzi\u0107 sobie\
  \ z niespodziewanymi sytuacjami w elegancki spos\xF3b. Robimy to, aby zarz\u0105\
  dza\u0107 b\u0142\u0119dami bez siwienia w\u0142os\xF3w\u2026"
lastmod: '2024-02-25T18:49:34.222515-07:00'
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w pozwala skryptowi radzi\u0107 sobie z niespodziewanymi\
  \ sytuacjami w elegancki spos\xF3b. Robimy to, aby zarz\u0105dza\u0107 b\u0142\u0119\
  dami bez siwienia w\u0142os\xF3w\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obsługa błędów pozwala skryptowi radzić sobie z niespodziewanymi sytuacjami w elegancki sposób. Robimy to, aby zarządzać błędami bez siwienia włosów naszych użytkowników.

## Jak to zrobić:
Aby przechwytywać błędy w Fish, użyj komendy `status` i warunków. Powiedzmy, że `ping` nie powiedzie się; oto jak to wykryć:

```fish
ping -c 1 example.com
if not status is-success
    echo "Coś rybiego stało się z pingiem."
end
```

Przykładowy wynik, jeśli `ping` się nie powiedzie:

```
Coś rybiego stało się z pingiem.
```

Aby obsłużyć konkretny kod błędu, użyj `status --is`:

```fish
false
if status --is 1
    echo "Przechwycono błąd z kodem 1."
end
```

Przykładowy wynik:
```
Przechwycono błąd z kodem 1.
```

Do bardziej zaawansowanego podejścia rozważ użycie funkcji:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping nie powiódł się ze statusem $status"
        return 1
    end
end

try_ping
```

## Glebsze zaglądanie
Obsługa błędów w Fish nie odpowiada paradygmatowi `try/catch`, który może być znany z języków wyższego poziomu. Zamiast tego mamy do czynienia z prostymi kodami wyjścia dostarczanymi przez komendę `status`.

Historycznie, w systemach podobnych do Unix, status wyjścia `0` oznacza sukces, podczas gdy każda inna wartość niezerowa wskazuje na błąd, który często odzwierciedla różne przyczyny niepowodzenia. Ta konwencja jest stosowana przez większość narzędzi linii poleceń i zatem, również przez Fisha.

Alternatywy dla sprawdzania `status` w Fish obejmują obsługę sygnałów przez `trap` w innych shellach, ale Fish preferuje bardziej wyraźne sprawdzanie statusu, ponieważ jest to czyściejsze i mniej podatne na efekty uboczne.

Pod względem implementacji, obsługa błędów w Fish pozostaje prosta, a zarazem potężna, przede wszystkim dzięki jej nieblokującej naturze i naciskowi na przejrzystą składnię, jak pokazano w przykładach. Kody błędów ładnie współgrają z funkcjami, pozwalając na modułowe i czytelne zarządzanie błędami.

## Zobacz również
- Dokumentacja Fisha o warunkach: https://fishshell.com/docs/current/language.html#conditionals
- Samouczek Fisha o obsłudze błędów: https://fishshell.com/docs/current/tutorial.html#error-handling
