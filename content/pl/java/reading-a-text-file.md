---
title:                "Odczytanie pliku tekstowego"
html_title:           "Java: Odczytanie pliku tekstowego"
simple_title:         "Odczytanie pliku tekstowego"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programujesz w języku Java, prawdopodobnie już wiesz, jak ważne są pliki tekstowe w przetwarzaniu danych. W tym artykule dowiecie się, jak czytać pliki tekstowe w prosty i skuteczny sposób.

## Jak to zrobić?

Czytanie pliku tekstowego w języku Java jest stosunkowo proste. Wystarczy wykorzystać klasę `FileReader` i `BufferedReader`, aby wczytać plik, a następnie użyć pętli `while` do odczytania danych linia po linii. Oto przykładowy kod:

```Java
FileReader file = new FileReader("plik.txt");
BufferedReader reader = new BufferedReader(file);

String line = reader.readLine();
while (line != null) {
    System.out.println(line);
    line = reader.readLine();
}

reader.close();
```

W tym kodzie najpierw tworzymy obiekt `FileReader`, który reprezentuje plik tekstowy `plik.txt`. Następnie tworzymy obiekt `BufferedReader`, który przetwarza dane z pliku i przechowuje je w pamięci podręcznej, co pozwala nam na bardziej efektywne czytanie. W pętli `while` czytamy pojedyncze linie z pliku, aż do momentu, gdy osiągniemy koniec pliku (wyrażenie `line != null`). Na końcu zamykamy obiekt `BufferedReader` przy użyciu metody `close()`.

Jeśli chcesz odczytać cały plik jako jeden ciąg znaków, możesz skorzystać z klasy `Scanner`, która posiada wiele przydatnych metod do przetwarzania danych tekstowych. Oto jak to zrobić:

```Java
Scanner scanner = new Scanner(new File("plik.txt"));
String content = scanner.useDelimiter("\\Z").next();
System.out.println(content);
scanner.close();
```

W tym przypadku tworzymy obiekt `Scanner`, który przyjmuje jako argument obiekt `File`, reprezentujący nasz plik tekstowy. Następnie używamy metody `useDelimiter()` do ustawienia separatora jako `\Z`, co oznacza koniec tekstu. W ten sposób podajemy całą zawartość pliku jako pojedynczy ciąg znaków do zmiennej `content`, którą następnie możemy wyświetlić na ekranie. Na końcu zamykamy obiekt `Scanner` przy użyciu metody `close()`.

## Deep Dive

Podczas czytania pliku tekstowego, istnieje wiele przydatnych metod, które mogą ułatwić przetwarzanie danych. Na przykład, możesz użyć metody `split()` klasy `String`, aby rozdzielić linię tekstu na pojedyncze wartości oddzielone określonym znakiem. Używając pętli `for`, możesz łatwo przetworzyć każdą z tych wartości. Oto przykładowy kod:

```Java
String line = "1, 2, 3, 4, 5";
String[] values = line.split(",");
for (String value : values) {
    System.out.println(value);
}
```

Wynik:

```
1
2
3
4
5
```

Inną przydatną klasą jest `LineNumberReader`, która pozwala na odczytywanie nie tylko zawartości pliku, ale także numerów kolejnych linii. Możesz tego użyć do wyświetlania numerów linii wraz z ich zawartością i w ten sposób ułatwić użytkownikowi odnalezienie konkretnych danych. Oto przykładowy kod:

```Java
LineNumberReader reader = new LineNumberReader(new FileReader("plik.txt"));
String line;
while ((line = reader.readLine()) != null) {
    System.out.println(reader.getLineNumber() + ": " + line);
}
reader.close();
```

Wynik:

```
1: Pierwsza linia tekstu
2: Druga linia tekstu
3: Trzecia linia tekstu
```