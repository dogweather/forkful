---
title:    "Java: Läsning av kommandoradsargument"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

---
## Varför

Att läsa in argument från kommandoraden är en vanlig uppgift inom Java-programmering. Genom att lära sig hur man gör detta kan du få en bättre förståelse för hur ditt program interagerar med användaren och hur du kan göra det mer anpassningsbart.

## Hur man gör det

Att läsa in kommandoradsargument i Java är enkelt och kräver bara några få steg.

Först måste du skapa ett String-objekt för att lagra argumenten. Du kan sedan använda Java-klassen Scanner för att läsa in dessa argument från kommandoraden.

Här är ett kodexempel som visar hur du kan läsa in och skriva ut en enkel sträng från kommandoraden:

```Java
import java.util.Scanner;

public class CommandLineArgumentsExample {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Skriv in ett meddelande: ");
        String message = scanner.nextLine();
        System.out.println("Ditt meddelande är: " + message);
    }
}
```

Om du till exempel kör detta program med argumentet "Hej!", kommer du få följande utskrift:

```
Skriv in ett meddelande: Hej!
Ditt meddelande är: Hej!
```

Du kan också läsa in flera argument genom att använda en loop och Scanner-metoden `next()`, vilket skulle se ut så här:

```Java
for (int i = 0; i < args.length; i++) {
    String argument = scanner.next();
    // Gör något med argumentet här
}
```

Om du vill använda argumenten som numeriska värden måste du ändra typen från String till antingen int eller double. Detta kan göras med hjälp av metoderna `Integer.parseInt()` eller `Double.parseDouble()`.

## Fördjupa dig

För att verkligen förstå hur kommandoradsargument fungerar är det viktigt att förstå några grundläggande koncept.

Först och främst är det viktigt att komma ihåg att alla kommandoradsargument är strängar, oavsett om de representerar numeriska värden eller inte. Det är därför du måste använda de nämnda metoderna för att konvertera dem till rätt typ.

Vidare kan du komma ihåg att argumenten alltid lagras i en array av strängar, där det första argumentet är på index 0, det andra på index 1 osv.

Du kan också komma ihåg att kommandoradsargumenten är skilda åt med mellanslag, så om du vill läsa in en fråga eller ett citat med flera ord behöver du använda citationstecken runt det för att det ska läsas in som en enda sträng.

Genom att förstå dessa koncept kan du läsa in och använda kommandoradsargument på ett mer effektivt och säkert sätt.

---

## Se också

- [Java Scanner-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Integer-klassen](https://docs.oracle.com/javase/8/docs/api/java/lang/Integer.html)
- [Double-klassen](https://docs.oracle.com/javase/8/docs/api/java/lang/Double.html)