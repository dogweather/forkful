---
title:    "C: Die Verwendung von regulären Ausdrücken"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind eine mächtige Methode, um Text in C-Programmen zu verarbeiten. Mit regulären Ausdrücken können Sie Text auf Muster überprüfen und bestimmte Zeichenfolgen suchen und ersetzen. Sie sind besonders nützlich, wenn Sie mit Benutzereingaben oder Dateien arbeiten, in denen Sie nach bestimmten Informationen suchen müssen.

## Wie geht man vor

Um reguläre Ausdrücke in C zu verwenden, müssen Sie zunächst die Header-Datei `regex.h` einbinden. Hier ist ein Beispiel, wie Sie einen regulären Ausdruck in Ihrem Code verwenden können:

```C 
// Wir definieren hier ein einfaches Muster 
char *pattern = "Hallo (Welt)"; 

// Wir definieren auch einen Eingabetext 
char *input = "Hallo Welt, wie geht es dir?"; 

// Erstelle eine Variable vom Typ regex_t, um den regulären Ausdruck zu speichern 
regex_t regex;

// Wir kompilieren den regulären Ausdruck mit dem kompilierten Flag REG_EXTENDED 
int result = regcomp(&regex, pattern, REG_EXTENDED); 

// Wenn die Kompilierung erfolgreich war, können wir die Übereinstimmungen suchen 
if(result == 0){ 
    // Wir definieren eine Match-Variable vom Typ regmatch_t 
    regmatch_t match; 

    // Mit der Funktion regexec können wir nach dem Muster in unserem Eingabetext suchen 
    result = regexec(&regex, input, 1, &match, 0); 

    // Wir können jetzt die Ergebnisse überprüfen 
    if(result == 0){ 
        // Die Übereinstimmung befindet sich in match.rm_so und match.rm_eo 
        printf("Das Muster wurde gefunden zwischen den Zeichen %d und %d des Eingabetextes!", match.rm_so, match.rm_eo); 
    } 
} 

// Wir müssen am Ende unsere regex-Variablen freigeben 
regfree(&regex); 
```

Die Ausgabe dieses Codes sollte folgendermaßen aussehen: `Das Muster wurde gefunden zwischen den Zeichen 6 und 15 des Eingabetextes!` Dies ist die Position des Musters "Welt" in unserem Eingabetext.

## Tiefergehende Informationen

Reguläre Ausdrücke können noch viel mehr als nur einfache Mustererkennung. Sie können auch verwendet werden, um Untergruppen zu definieren, die in der Übereinstimmung zurückgegeben werden sollen, oder um Zeichenklassen wie Zahlen oder Buchstaben zu erstellen. Eine ausführliche Anleitung zur Verwendung von regulären Ausdrücken in C finden Sie in der offiziellen Dokumentation der `regex.h`-Header-Datei.

## Siehe auch

- Die offizielle Dokumentation der `regex.h` Header-Datei: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html