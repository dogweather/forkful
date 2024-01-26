---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:14:18.587039-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?

En REPL, en förkortning för Read-Eval-Print Loop, är ett programmeringsverktyg för att interaktivt köra kod och se resultat direkt. Programmerare använder den för att experimentera, felsöka eller lära sig ett nytt språk på flygande fot som Gleam.

## Hur:

Gleam inkluderar för närvarande inte en REPL i sin standarddistribution. Du kan dock experimentera med Gleam-kod genom att använda den befintliga Erlang-shellen eftersom Gleam kompilerar till Erlang-bytekod. Så här gör du:

1. Kompilera din Gleam-kod till Erlang.
```plaintext
gleam build
```

2. Starta Erlang-shellen.
```plaintext
erl -pa ebin
```

3. Anropa dina Gleam-funktioner (om du antar att du har en modul som heter `my_mod` och funktion `my_fun`).
```erlang
my_mod:my_fun().
```

Du bör se utmatningen av din funktion visas i shellen.

## Fördjupning

REPL förkroppsligar den dynamiska och utforskande andan hos många funktionella programmeringsspråk, med rötter som går tillbaka till LISP's REPL på 1960-talet. Jämförelsevis erbjuder andra system som Python's `ipython` eller Ruby's `irb` liknande upplevelser för deras samhällen.

Även om Gleam inte har en inbyggd REPL ännu, är att utnyttja Erlang-shellen ett klurigt förhållningssätt. Erlang-shellens kapaciteter kommer från BEAM VM, den virtuella maskinen som driver Erlang-ekosystemet, vilket inkluderar Elixir, LFE och Gleam.

Alternativ till REPLs i Gleam-ekosystemet kan inkludera att skriva testfall eller använda onlinekompilatorer och kodlekområden som stöder Gleam, för att testa kodsnuttar utanför en fullständig projektuppsättning.

Implementeringen av en dedikerad Gleam-REPL står inför utmaningar huvudsakligen kring den kompilerade naturen av Gleam och Erlangs körningsmiljö, där byte av kod i drift är normen. En framtida Gleam-REPL skulle behöva förena språkets statiska typning med den dynamiska exekveringsmiljö som en REPL förväntar sig.

## Se även

- Gleams officiella dokumentation: https://gleam.run/book/
- Dokumentation för Erlangs shell: http://erlang.org/doc/man/erl.html
- En online Gleam-kompilator och lekplats: https://gleam.run/compiler/