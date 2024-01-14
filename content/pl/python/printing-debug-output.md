---
title:    "Python: Dr"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

Debugowanie jest nieodłączną częścią każdego programisty. Jednak często napotykamy na trudności, szczególnie w większych projektach, gdy nie wiemy co dokładnie dzieje się w naszej aplikacji lub co jest przyczyną błędu. Wtedy przydatne staje się wypisywanie wyników naszego kodu, czyli tzw. "debug output". W tym artykule dowiesz się, dlaczego warto sięgać po tę metodę oraz jak ją zastosować w praktyce.

## Jak

```python
# Przykładowy kod z wykorzystaniem print()
x = 5
print(f"Wartość x wynosi: {x}")
```

Wykorzystanie funkcji `print()` jest najprostszym sposobem na wypisanie wartości zmiennych lub komunikatów w naszym kodzie. Możemy również wykorzystać funkcję `pdb.set_trace()` w celu tworzenia punktów zatrzymań, które pozwolą nam prześledzić dokładnie przebieg naszego programu.

## Deep Dive

Istnieje wiele sposobów na wypisywanie debug output w Pythonie. Możemy korzystać z różnego rodzaju narzędzi, takich jak logger czy debugger. Jednak w przypadku prostych skryptów, wykorzystanie funkcji `print()` jest wystarczające.

Warto również wiedzieć, że funkcja `print()` może przyjmować wiele argumentów, co pozwala na wygodne wypisywanie wielu wartości jednocześnie. Możemy również korzystać z instrukcji warunkowych i pętli, aby kontrolować, które wartości zostaną wypisane.

## Zobacz również

- [Dokumentacja Pythona - Debugging and Profiling](https://docs.python.org/3.8/library/pdb.html)
- [10 przydatnych trików dla Pythonistów](https://realpython.com/python-tricks/)
- [Ten sposób drukowania obiektów Pythona jest taki uroczy](https://docs.python.org/3/library/functions.html#print)