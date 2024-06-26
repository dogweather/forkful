---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:01.393800-07:00
description: "Jak to zrobi\u0107: Aby sprawdzi\u0107, czy ci\u0105g pasuje do wzorca,\
  \ mo\u017Cesz u\u017Cy\u0107 `grep`, narz\u0119dzia wiersza polece\u0144 do wyszukiwania\
  \ zestaw\xF3w danych w formie zwyk\u0142ego\u2026"
lastmod: '2024-03-13T22:44:35.572595-06:00'
model: gpt-4-0125-preview
summary: "Aby sprawdzi\u0107, czy ci\u0105g pasuje do wzorca, mo\u017Cesz u\u017C\
  y\u0107 `grep`, narz\u0119dzia wiersza polece\u0144 do wyszukiwania zestaw\xF3w\
  \ danych w formie zwyk\u0142ego tekstu dla linii, kt\xF3re pasuj\u0105 do wyra\u017C\
  enia regularnego."
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
weight: 11
---

## Jak to zrobić:


### Podstawowe dopasowywanie wzorców
Aby sprawdzić, czy ciąg pasuje do wzorca, możesz użyć `grep`, narzędzia wiersza poleceń do wyszukiwania zestawów danych w formie zwykłego tekstu dla linii, które pasują do wyrażenia regularnego:

```bash
echo "Hello, World!" | grep -o "World"
# Wyjście: World
```

### Ekstrakcja konkretnych danych
Aby wyodrębnić części danych, które pasują do twoich wzorców regex, możesz użyć `-o` z `grep`:

```bash
echo "Error: File not found" | grep -oE "[A-Za-z]+:"
# Wyjście: Error:
```

### Użycie Regex z `sed`
`sed` (edytor strumieniowy) to potężne narzędzie do parsowania i transformacji tekstu. Oto jak używać `sed` z regexem do zamiany tekstu:

```bash
echo "Bash jest świetny" | sed -e 's/świetny/wspaniały/'
# Wyjście: Bash jest wspaniały
```

### Dopasowywanie wzorców w instrukcjach warunkowych
Bash również obsługuje regex bezpośrednio w instrukcjach warunkowych:

```bash
[[ "https://example.com" =~ ^https?:// ]] && echo "URL jest poprawny" || echo "URL jest niepoprawny"
# Wyjście: URL jest poprawny
```

### Zaawansowane dopasowywanie wzorców i manipulacja z `awk`
`awk` to kolejne narzędzie do przetwarzania tekstu, które obsługuje bardziej złożone ekstrakcje i manipulacje danymi. Może być szczególnie przydatny podczas pracy z uporządkowanymi danymi tekstowymi, takimi jak CSV:

```bash
echo -e "ID,Imię,Wiek\n1,Jan,22\n2,Anna,24" | awk -F, '$3 > 22 {print $2 " jest starszy(a) niż 22."}'
# Wyjście: Anna jest starsza niż 22.
```

Chociaż wbudowane funkcje regex w Bashu pokrywają wiele przypadków użycia, dla bardzo zaawansowanych operacji regex, możesz rozważyć użycie kombinacji skryptów Bash z skryptami `perl` lub `python`, ponieważ te języki oferują potężne biblioteki regex (np. `re` w Pythonie). Prosty przykład z Pythonem:

```bash
echo "Capture this 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# Wyjście: 123
```

Incorporowanie tych języków programowania w razie potrzeby może pomóc wykorzystać pełną moc regex w twoich skryptach Bash.
