---
date: 2024-01-20 18:03:33.467600-07:00
description: "Att starta ett nytt projekt i Elixir inneb\xE4r att skapa en ren arbetsyta\
  \ f\xF6r din kod. Programmerare g\xF6r det f\xF6r att f\xE5 en strukturerad b\xF6\
  rjan och\u2026"
lastmod: 2024-02-19 22:04:56.813987
model: gpt-4-1106-preview
summary: "Att starta ett nytt projekt i Elixir inneb\xE4r att skapa en ren arbetsyta\
  \ f\xF6r din kod. Programmerare g\xF6r det f\xF6r att f\xE5 en strukturerad b\xF6\
  rjan och\u2026"
title: "Att p\xE5b\xF6rja ett nytt projekt"
---

{{< edit_this_page >}}

## Vad & Varför?
Att starta ett nytt projekt i Elixir innebär att skapa en ren arbetsyta för din kod. Programmerare gör det för att få en strukturerad början och identifiera vilka byggstenar som behövs.

## Så här gör du:
För att skapa ett nytt Elixir-projekt, använd `mix`, Elixirs byggverktyg. 

```elixir
# Installera Elixir om det inte redan är gjort, använd en versionhanterare som asdf eller kerl om du vill ha flera versioner.
# Öppna terminalen och skriv:

mix new mitt_projekt
# Ersätt 'mitt_projekt' med ditt projektnamnsval.

# Kör kommandot och du får något liknande:
```
```elixir
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/mitt_projekt.ex
* creating test
* creating test/test_helper.exs
* creating test/mitt_projekt_test.exs

Your Mix project was created successfully.
You can use "mix" to compile it, test it, and more.

# Navigera till din nya projektkatalog:
cd mitt_projekt

# För att köra projektet och testerna:
mix test
```
```elixir
Compiling 1 file (.ex)
Generated mitt_projekt app
.

Finished in 0.02 seconds
1 test, 0 failures

Randomized with seed 54321
```

## Deep Dive
Elixir släpptes för första gången 2011 av José Valim. `mix new` är ett kommando som skapade historia genom att förenkla skapandet av nya Elixir-projekt. Alternativen inkluderar att manuellt strukturera mappar och filer eller kopiera en befintlig projektmall. Implementeringsdetaljerna är att `mix` automatiskt skapar nödvändiga filer som `mix.exs` för projektets konfiguration och dependency management.

## See Also
- [Elixir Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html) – Introduktion till Elixir.
- [Mix Tool Documentation](https://hexdocs.pm/mix/Mix.html) – Djupgående dokumentation för `mix`.
- [Elixir School](https://elixirschool.com/en/) – Lektioner om Elixirs grundläggande och avancerade koncept.
- [Hex.pm](https://hex.pm/) – Pakethanterare för Elixir för att utforska och inkludera externa bibliotek och verktyg.
