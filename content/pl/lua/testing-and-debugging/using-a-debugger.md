---
date: 2024-01-26 03:50:31.284215-07:00
description: "Debugger to narz\u0119dzie, kt\xF3re pozwala inspekcjonowa\u0107 i kontrolowa\u0107\
  \ wykonanie programu, u\u0142atwiaj\u0105c wskazanie momentu, kiedy co\u015B idzie\
  \ nie tak. Programi\u015Bci\u2026"
lastmod: '2024-03-13T22:44:35.544501-06:00'
model: gpt-4-0125-preview
summary: "Debugger to narz\u0119dzie, kt\xF3re pozwala inspekcjonowa\u0107 i kontrolowa\u0107\
  \ wykonanie programu, u\u0142atwiaj\u0105c wskazanie momentu, kiedy co\u015B idzie\
  \ nie tak. Programi\u015Bci\u2026"
title: Korzystanie z debugera
---

{{< edit_this_page >}}

## Co i dlaczego?
Debugger to narzędzie, które pozwala inspekcjonować i kontrolować wykonanie programu, ułatwiając wskazanie momentu, kiedy coś idzie nie tak. Programiści używają debuggerów do usuwania błędów, zrozumienia przepływu kodu oraz aby upewnić się, że ich kod jest czysty jak gwizdek.

## Jak to zrobić:
Lua nie posiada wbudowanego debuggera, ale można używać zewnętrznych, na przykład ZeroBrane Studio. Oto mała próbka pracy z nim:

```Lua
-- To jest prosty skrypt Lua z zamierzonym błędem
local function add(a, b)
    local result = a + b -- Ups, udajmy, że zapomnieliśmy zdefiniować 'b'
    return result
end

print(add(10))
```

Gdy uruchomisz to w debuggerze, wykonanie zostanie zatrzymane w miejscu, gdzie coś poszło nie tak. Zobaczysz coś takiego:

```
lua: example.lua:3: próba wykonania działania arytmetycznego na wartości nil (lokalny 'b')
ślad stosu:
	example.lua:3: w funkcji 'add'
	example.lua:7: w głównym bloku
	[C]: w ?
```

Możesz ustawić punkty przerwania, krokować przez kod i zerknąć na wartości zmiennych, aby wyśledzić błąd, nie tracąc przy tym zmysłów.

## Pogłębione rozważania
Niestety, prostota Lua nie rozciąga się na debugowanie. Bez obaw jednak, społeczność Lua staje na wysokości zadania. Narzędzia takie jak ZeroBrane Studio, LuaDec i inne oferują możliwości debugowania. Historycznie rzecz biorąc, debuggery pojawiły się nie długo po tym, jak pierwsze programy zaczęły sprawiać problemy, dając programistom środki do naprawy swojego kodu bez ślepego majstrowania.

W przypadku Lua często opierasz się na zewnętrznych debuggerach lub budujesz je w swoje środowisko programistyczne. Na przykład ZeroBrane Studio to środowisko IDE, które w pełni integruje debugger Lua. Pozwala to krokować przez kod, ustawiać punkty przerwania i obserwować zmienne. Z punktu widzenia implementacji, debuggery zazwyczaj używają haków do wstawiania punktów przerwania i innych udogodnień debugowania.

Alternatywy? Oczywiście. Dobre, stare instrukcje `print`, pieszczotliwie nazywane "debugowaniem printf", czasami mogą załatwić sprawę bez wyrafinowanych narzędzi.

## Zobacz również
Aby kontynuować swoją przygodę z debugowaniem, sprawdź:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Wiki użytkowników Lua na temat debugowania kodu Lua: http://lua-users.org/wiki/DebuggingLuaCode
- Referencję biblioteki `debug` w manualu Lua: https://www.lua.org/manual/5.4/manual.html#6.10
