---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "C#: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Was ist das und Warum?
Using regular expressions in your code allows you to search for patterns in strings of text. It's a powerful tool that saves time and effort for programmers by providing an efficient way to manipulate and extract data from text. 

# Wie geht es?
## Beispiele
Hier sind einige Beispiele, um Ihnen einen Einblick zu geben, wie reguläre Ausdrücke in C# verwendet werden können:

### Beispiel 1: Telefonnummer validieren
```
using System;
using System.Text.RegularExpressions;

// Regular expression for a 10-digit phone number
Regex phoneNumberRegex = new Regex(@"^\d{10}$");

// Valid phone number
string phoneNumber1 = "1234567890";
Console.WriteLine(phoneNumberRegex.IsMatch(phoneNumber1)); // Output: True

// Invalid phone number
string phoneNumber2 = "1234";
Console.WriteLine(phoneNumberRegex.IsMatch(phoneNumber2)); // Output: False
```

### Beispiel 2: E-Mail-Adresse prüfen
```
using System;
using System.Text.RegularExpressions;

// Regular expression for a basic email validation
Regex emailRegex = new Regex(@"^([\w\.\-]+)@([\w\-]+)((\.(\w){2,3})+)$");

// Valid email address
string email1 = "example@mail.com";
Console.WriteLine(emailRegex.IsMatch(email1)); // Output: True

// Invalid email address
string email2 = "example.com";
Console.WriteLine(emailRegex.IsMatch(email2)); // Output: False
```

### Beispiel 3: Zeichenfolgen extrahieren
```
using System;
using System.Text.RegularExpressions;

// Regular expression to extract the domain name from a URL
Regex urlRegex = new Regex(@"(https?://)?(www\.)?([\w-\.]+)");

// URL with "https://"
string url1 = "https://www.example.com";
Match match1 = urlRegex.Match(url1);
Console.WriteLine(match1.Groups[3].Value); // Output: "example.com"

// URL without "http"
string url2 = "www.google.com";
Match match2 = urlRegex.Match(url2);
Console.WriteLine(match2.Groups[3].Value); // Output: "google.com"
```

## Tiefergehende Informationen
### Historischer Kontext
Reguläre Ausdrücke wurden bereits in den 1950er Jahren entwickelt, als die Notation für reguläre Sprachen eingeführt wurde. Sie wurden dann in den 1960er Jahren von verschiedenen Programmiersprachen übernommen, darunter auch C#.

### Alternativen
Während reguläre Ausdrücke ein mächtiges Werkzeug für die Textverarbeitung sind, gibt es auch andere Möglichkeiten, bestimmte Aufgaben zu erledigen, z.B. durch die Verwendung von String-Manipulationsfunktionen oder einfachen Bedingungen.

### Implementierungsdetails
In C# können reguläre Ausdrücke mit der `Regex`-Klasse aus dem `System.Text.RegularExpressions`-Namespace verwendet werden. Diese Klasse bietet eine breite Palette von Möglichkeiten, um verschiedene Muster in Zeichenfolgen zu suchen und zu manipulieren.

## Siehe auch
- [Microsoft Documentation on Regular Expressions in C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions) 
- [Tutorial on Regex in C#](https://www.c-sharpcorner.com/article/regular-expressions-in-c-sharp/)