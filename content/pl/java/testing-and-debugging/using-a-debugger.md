---
date: 2024-01-26 03:49:52.247658-07:00
description: "Jak to zrobi\u0107: Za\u0142\xF3\u017Cmy, \u017Ce masz prosty program\
  \ Java, kt\xF3ry dzia\u0142a niepoprawnie, i nie mo\u017Cesz zrozumie\u0107 dlaczego.\
  \ Oto jak uruchomi\u0107 debuger przy u\u017Cyciu\u2026"
lastmod: '2024-03-13T22:44:35.281815-06:00'
model: gpt-4-0125-preview
summary: "Za\u0142\xF3\u017Cmy, \u017Ce masz prosty program Java, kt\xF3ry dzia\u0142\
  a niepoprawnie, i nie mo\u017Cesz zrozumie\u0107 dlaczego."
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
Załóżmy, że masz prosty program Java, który działa niepoprawnie, i nie możesz zrozumieć dlaczego. Oto jak uruchomić debuger przy użyciu Eclipse, jednego z popularnych środowisk IDE do rozwoju aplikacji Java:

Najpierw upewnij się, że ustawiłeś punkt przerwania. Następnie kliknij prawym przyciskiem myszy na plik, wybierz 'Debuguj jako' i kliknij na 'Aplikacja Java'.

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // Ustaw tutaj punkt przerwania
        int result = divide(a, b);
        System.out.println("Wynik to: " + result);
    }

    private static int divide(int licznik, int mianownik) {
        // Kolejne dobre miejsce na punkt przerwania
        return licznik / mianownik;
    }
}
```

Robiąc to, twój program zatrzyma się na punkcie przerwania, i będziesz mógł inspekcjonować zmienne, przechodzić przez kod linia po linii i obserwować, jak zachowuje się twój program.

Przykładowe wyjście (w konsoli debugera):
```
Osiągnięto punkt przerwania na linii: int result = divide(a, b);
```

## Zanurzenie się głębiej
Koncepcja debugowania istnieje od wczesnych dni programowania. Legenda głosi, że termin "bug" (błąd) pojawił się faktycznie od realnego, ćmowego błęda znalezionego wewnątrz komputera przez Grace Hopper, pionierkę w tej dziedzinie. Przesuwając się do dziś, mamy zaawansowane środowiska IDE takie jak IntelliJ IDEA, Eclipse i NetBeans, które zawierają potężne debugery.

Alternatywy dla debuggerów IDE to logowanie, instrukcje print (tzw. debugger biedaka), asercje oraz samodzielne narzędzia do debugowania jak jdb (Java Debugger), które jest częścią Java Development Kit (JDK).

Debugger działa, pozwalając programiście na pauzowanie wykonania (punkty przerwania), krokowe przechodzenie przez kod, inspekcję wartości zmiennych, modyfikację tych wartości w locie i nawet wykonywanie bloków kodu po kolei. Użycie debugera jest często uważane za nieocenioną technikę przy tworzeniu złożonych aplikacji, gdzie odnalezienie dokładnej linii kodu powodującej problem można porównać do szukania igły w stogu siana.

## Zobacz także
- Oficjalna dokumentacja Oracle dotycząca debugowania: [Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- Przewodnik po debugowaniu w Eclipse: [Porady dotyczące debugowania w Eclipse](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- VisualVM, narzędzie wizualne integrujące kilka narzędzi wiersza poleceń z JDK i lekkie możliwości profilowania: [VisualVM](https://visualvm.github.io/)
