---
date: 2024-01-27 20:32:52.201505-07:00
description: "Generowanie losowych liczb w Bashu zapewnia spos\xF3b na wprowadzenie\
  \ nieprzewidywalno\u015Bci do skrypt\xF3w, co jest niezb\u0119dne do zada\u0144\
  \ takich jak generowanie\u2026"
lastmod: '2024-03-13T22:44:35.578540-06:00'
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w Bashu zapewnia spos\xF3b na wprowadzenie nieprzewidywalno\u015B\
  ci do skrypt\xF3w, co jest niezb\u0119dne do zada\u0144 takich jak generowanie\u2026"
title: Generowanie liczb losowych
weight: 12
---

## Co i dlaczego?
Generowanie losowych liczb w Bashu zapewnia sposób na wprowadzenie nieprzewidywalności do skryptów, co jest niezbędne do zadań takich jak generowanie bezpiecznych haseł, symulowanie danych czy programowanie gier. Programiści wykorzystują tę możliwość, aby dodać zmienność do swoich skryptów lub testować swoje programy w różnorodnych, losowo generowanych warunkach.

## Jak to zrobić:
W Bashu zmienna `$RANDOM` jest głównym narzędziem do generowania losowych liczb. Za każdym razem, gdy się do niej odwołasz, Bash dostarcza pseudolosową liczbę całkowitą między 0 a 32767. Oto kilka praktycznych przykładów:

```Bash
# Podstawowe użycie $RANDOM
echo $RANDOM

# Generowanie losowej liczby w określonym zakresie (tutaj 0-99)
echo $(( RANDOM % 100 ))

# Generowanie bardziej "bezpiecznej" losowej liczby, odpowiedniej dla haseł lub kluczy
# Użycie /dev/urandom z poleceniem od
head -c 8 /dev/urandom | od -An -tu4

# Inicjowanie wartości RANDOM dla powtarzalności
RANDOM=42; echo $RANDOM
```

Przykładowe wyjście (uwaga: rzeczywiste wyniki będą się różnić, ponieważ liczby są losowe):
```Bash
16253
83
3581760565
17220
```

## Szczegółowa analiza
Mechanizm stojący za `$RANDOM` w Bashu generuje liczby pseudolosowe, co oznacza, że podążają one za algorytmem i w teorii mogą być przewidywalne - potencjalna wada bezpieczeństwa dla aplikacji wymagających autentycznej nieprzewidywalności. Współczesne aplikacje kryptograficzne zazwyczaj wymagają losowości pochodzącej z zjawisk fizycznych lub z sprzętu zaprojektowanego specjalnie do generowania danych losowych, takich jak `/dev/urandom` lub `/dev/random` w systemach Linux, które zbierają szum otoczenia.

Dla zadań niekrytycznych pod względem bezpieczeństwa lub przypadkowych, `$RANDOM` wystarcza i oferuje korzyść prostoty. Jednak w przypadku zastosowań kryptograficznych lub tam, gdzie jakość losowości jest kluczowa, programiści powinni zwrócić się w stronę innych narzędzi i języków zaprojektowanych z myślą o kryptografii, takich jak OpenSSL czy języki programowania z solidnymi bibliotekami generatorów liczb losowych.

Chociaż `$RANDOM` Bash-a spełnia swoją rolę w skryptach wymagających podstawowych liczb losowych, jego ograniczenia powinny skłonić programistów do poszukiwania bardziej solidnych rozwiązań dla aplikacji, gdzie jakość lub bezpieczeństwo losowości ma znaczenie.
