---
title:                "Att skriva en textfil"
html_title:           "Java: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

Vad & Varför? 
Skriva en textfil är när man använder kod för att skapa en fil som kan innehålla text. Programmet använder kod för att skriva den önskade texten till filen och sparar den för framtida användning. Programmörer gör detta för att spara viktig information, dela data med andra eller skapa en loggfilsom kan användas för att spåra programmet.
Hur gör man:
Java-kod för att skapa en textfil och skriva till den:

```
import java.io.*;
public class TextFil {

   public static void main(String args[]) {
      try {
         // Skapar en ny textfil 
         File textFil = new File("mittTextdokument.txt");
         
         // Skapar en ny FileWriter-object
         FileWriter skrivare = new FileWriter(textFil);
         
         // Skriver text till filen
         skrivare.write("Det här är min första textfil!");
         
         // Stänger skrivaren
         skrivare.close();
         
         // Skriver ett meddelande om att filen skapades
         System.out.println("En ny textfil har skapats!");
         
      } catch(IOException e) {
         e.printStackTrace();
      }
   }
}

```

Vad händer:
Kodexemplet ovan kommer att skapa en ny fil med namnet "mittTextdokument.txt" och skriva texten "Det här är min första textfil!" till filen. Om koden körs mer än en gång kommer det att ersätta den tidigare texten med den nya texten.

Djupgående:
Skriva till en textfil är en vanlig uppgift inom programmering. Det ger användare möjlighet att spara data på ett enkelt och läsbart sätt. Det finns också andra sätt att spara data, som till exempel att använda en databastjänst. Men att skriva till en textfil är ett snabbt och enkelt sätt att spara information.

Se även:
- Java Fil Hantverk (https://www.programiz.com/java-programming/file-handling)
- Java Output strömmar (https://www.w3schools.com/java/java_files_create.asp)
- Programmering för nybörjare - Textfiler (https://www.programmeringforfeminarier.se/blogg/textfiler/)