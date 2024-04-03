---
date: 2024-01-26 03:39:55.639047-07:00
description: "Jak to zrobi\u0107: Wyrwijmy te irytuj\u0105ce cudzys\u0142owy z naszego\
  \ tekstu. Do szybkich napraw u\u017Cyjemy metody `replace()`, a do twardych orzech\xF3\
  w do zgryzienia -\u2026"
lastmod: '2024-03-13T22:44:35.265107-06:00'
model: gpt-4-0125-preview
summary: "Wyrwijmy te irytuj\u0105ce cudzys\u0142owy z naszego tekstu."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Wyrwijmy te irytujące cudzysłowy z naszego tekstu. Do szybkich napraw użyjemy metody `replace()`, a do twardych orzechów do zgryzienia - regex.

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"Hello, 'World'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // Hello, World!

        // Teraz z regex dla miłośników wzorców
        String stringWithMixedQuotes = "\"Java\" and 'Programming'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java and Programming
    }
}
```

## Głębsze spojrzenie
Kiedyś, cudzysłowy w ciągach znaków nie były dużym problemem — systemy były prostsze, a dane mniej skomplikowane. Z nadejściem złożonych formatów danych (JSON, XML) i potrzeby wymiany danych, zarządzanie cudzysłowami stało się kluczowe. Mówiąc o alternatywach, oczywiście można napisać parser, przejść przez każdy znak i zbudować nowy ciąg znaków (może być to zabawne w deszczowy dzień). Istnieją również biblioteki stron trzecich, które mogą sobie z tym poradzić bardziej wyszukanie, oferując opcje ucieczki znaków zamiast ich usuwania, lub obsługę różnych typów cudzysłowów zgodnie z lokalizacją. Pod względem implementacji, należy pamiętać, że usuwanie cudzysłowów bez kontekstu może zmienić znaczenie lub strukturę danych — zawsze należy rozważyć „dlaczego” przed „jak”.

## Zobacz także
- Aby zagłębić się w regex, sprawdź oficjalną dokumentację Java: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- Potrzebujesz ucieczki przed cudzysłowami zamiast ich usuwania? Stack Overflow przyjdzie z pomocą: https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- Przetwarzanie JSON w Javie? Prawdopodobnie często będziesz się spotykać z cudzysłowami. Oto punkt wyjścia: https://www.oracle.com/technical-resources/articles/java/json.html
