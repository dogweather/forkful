---
date: 2024-01-20 18:03:29.042748-07:00
description: "Rozpoczynanie nowego projektu to jak otwieranie nowego rozdzia\u0142\
  u: jest \u015Bwie\u017Ce, czyste i wszystko jest mo\u017Cliwe. Programi\u015Bci\
  \ zaczynaj\u0105 nowe projekty, by\u2026"
lastmod: 2024-02-19 22:04:54.222269
model: gpt-4-1106-preview
summary: "Rozpoczynanie nowego projektu to jak otwieranie nowego rozdzia\u0142u: jest\
  \ \u015Bwie\u017Ce, czyste i wszystko jest mo\u017Cliwe. Programi\u015Bci zaczynaj\u0105\
  \ nowe projekty, by\u2026"
title: Rozpoczynanie nowego projektu
---

{{< edit_this_page >}}

## What & Why (Co i Dlaczego?)
Rozpoczynanie nowego projektu to jak otwieranie nowego rozdziału: jest świeże, czyste i wszystko jest możliwe. Programiści zaczynają nowe projekty, by stworzyć coś unikalnego lub rozwiązać problem, każdorazowo kształtując kod do swoich potrzeb.

## How to (Jak to zrobić?)
Zakładając, że masz już zainstalowany Elixir i narzędzie do zarządzania pakietami Hex, zaczynasz od utworzenia nowej aplikacji za pomocą polecenia `mix`.

```Elixir
$ mix new my_app
```

Następnie wejdź do katalogu aplikacji:

```Elixir
$ cd my_app
```

By uruchomić aplikację i jej testy, użyj:

```Elixir
$ mix run
$ mix test
```

Powinieneś zobaczyć output typu:

```Elixir
Compiling 1 file (.ex)
Generated my_app app
...
1 test, 0 failures
```

## Deep Dive (Dogłębna analiza)
Elixir, stworzony przez José Valima, zainspirowany był językiem Erlang, który okazał się świetym rozwiązaniem dla systemów rozproszonych i aplikacji wymagających dużych obciążeń. Mix, narzędzie budowane wraz z Elixir, udostępnia szereg funkcji do zarządzania życiem projektu. Nie tylko może stworzyć nowy projekt, ale też zarządzać zależnościami i zadaniami.

Jako alternatywa, wielu programistów korzysta również z Phoenix – frameworka webowego w Elixir, który dostarcza więcej struktury i narzędzi specyficznych dla aplikacji internetowych.

Ważnym krokiem jest zarządzanie zależnościami projektu za pomocą pliku `mix.exs`. Umożliwia on określenie modułów i aplikacji, które są potrzebne, jak również ich wersji.

## See Also (Zobacz także)
- [Elixir Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html)
- [Mix & OTP Guide](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Phoenix Framework](https://www.phoenixframework.org/)
