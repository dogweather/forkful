---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie do standardowego błędu (stderr) to sposób na wysyłanie komunikatów o błędach i innych ważnych informacji, które nie są częścią głównego wyniku programu. Programiści robią to, aby oddzielić normalne dane wyjściowe od informacji o błędach, co ułatwia debugowanie i logowanie.

## Jak to zrobić:

```Java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("To jest normalne wyjście.");
        System.err.println("To jest wyjście błędu.");
    }
}
```

Przykładowe wyjście:
```
To jest normalne wyjście.
To jest wyjście błędu.
```

## Dogłębna analiza:

Pisanie do stderr sięga czasów Unixowych terminali, gdzie standardowo wyjście i błąd były przekierowywane do tych samych lub różnych miejsc. Alternatywą może być użycie własnych mechanizmów logowania, takich jak log4j. Podczas implementacji, `System.err` wiąże się z natywnym strumieniem wyjściowym języka operacyjnego, który jest specjalnie przeznaczony do obsługi błędów i ważnych ostrzeżeń.

## Zobacz również:

- Oficjalna dokumentacja Oracle na temat klas `System` i `PrintStream`: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html
- Tutorial na temat logowania w Java z wykorzystaniem log4j: https://logging.apache.org/log4j/2.x/manual/index.html
- Artykuł na Stack Overflow na temat różnic pomiędzy `System.out` a `System.err`: https://stackoverflow.com/questions/31394569/what-is-the-difference-between-system-out-and-system-err
