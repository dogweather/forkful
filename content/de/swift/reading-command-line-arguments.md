---
title:    "Swift: Lesen von Befehlszeilenargumenten"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Warum

Wenn du neu in der Welt der Swift-Programmierung bist, hast du dich vielleicht schon einmal gefragt, warum man sich mit Befehlszeilenargumenten beschäftigen sollte. Die Antwort ist einfach: Das Lesen von Befehlszeilenargumenten ermöglicht es dir, deinem Programm mehr Flexibilität zu geben und es an verschiedene Eingaben anzupassen.

## Wie man Befehlszeilenargumente liest

Um Befehlszeilenargumente in deinem Swift-Programm zu lesen, musst du zunächst die Argumente über die `CommandLine`-Klasse aufrufen. Hier ist ein Beispielcode:

```Swift 
let arguments = CommandLine.arguments 
```

Dieser Code erstellt eine Variable namens `arguments`, die alle Befehlszeilenargumente enthält.

Um die Argumente im Detail zu untersuchen, kannst du eine Schleife verwenden, um durch sie zu iterieren. Hier ist ein Beispiel, das die Argumente ausgibt:

```Swift 
for arg in arguments { 
    print(arg) 
} 
```

Wenn du nun dein Programm startest und Befehlszeilenargumente eingibst, werden diese auf der Konsole ausgegeben. Zum Beispiel, wenn du `./Programm Argument1 Argument2` eingibst, wird die Ausgabe folgendermaßen aussehen:

```
./Programm 
Argument1 
Argument2
```

## Tiefer Einblick: Das Lesen von Befehlszeilenargumenten

Das Lesen von Befehlszeilenargumenten kann noch mächtiger werden, wenn du optionalere Argumente benötigst. Du kannst zum Beispiel Standardwerte für bestimmte Argumente setzen, falls keine Eingabe erfolgt. Hier ist ein Beispielcode, der eine Funktion `getArgument` definiert, die ein bestimmtes Argument zurückgibt oder einen Standardwert, falls keines angegeben wurde:

```Swift 
func getArgument(_ argument:String, withDefault: String) -> String { 
    if let index = arguments.firstIndex(of: argument), index + 1 < arguments.count { 
        return arguments[index+1] 
    } else { 
        return withDefault 
    } 
} 
```

Du kannst diese Funktion dann verwenden, um ein Argument abzufragen:

```Swift 
let argument1 = getArgument("-a", withDefault: "default") 
```

Dieses Code würde das erste Argument zurückgeben, das mit `-a` gekennzeichnet ist. Falls kein Argument angegeben wird, wird der Standardwert "default" zurückgegeben.

## Siehe auch 
- [Apple Dokumentation: CommandLine Class](https://developer.apple.com/documentation/foundation/commandline)
- [Swift by Sundell: Working with the Command Line in Swift](https://www.swiftbysundell.com/tips/working-with-the-command-line-in-swift/)
- [Hacking with Swift: How to read command line arguments using Swift 2.0](https://www.hackingwithswift.com/read/0/8/reading-command-line-arguments-using-swift-20)