---
title:    "Ruby: Å skrive en tekstfil"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor 
 
Å skrive en tekstfil er en enkel måte å lagre informasjon på i et format som er lett å lese og endre. Dette er veldig praktisk når du trenger å lagre data eller tekst som ikke trenger et mer komplisert lagringsformat, som for eksempel en database. Det er også en god måte å organisere og lagre kode på, slik at den kan brukes igjen senere. 
 
## Hvordan
 
For å skrive en tekstfil i Ruby, kan du bruke `File` klassen. Først må du åpne en fil ved å bruke `File.open` metoden og angi filnavnet og modusen du ønsker å åpne filen i. Modusen kan være "w" for å skrive til filen eller "a" for å legge til data til slutten av filen. Deretter kan du bruke `.puts` metoden for å skrive data til filen, og `.close` metoden for å lukke filen når du er ferdig. Her er et eksempel på hvordan du kan skrive tekst til en fil ved hjelp av Ruby: 
 
```Ruby 
f = File.open("tekstfil.txt", "w") 
f.puts("Dette er en tekstfil skrevet med Ruby.") 
f.close 
``` 
Output: 
`Dette er en tekstfil skrevet med Ruby.` 
 
Det er også andre metoder du kan bruke for å skrive til tekstfilen, som for eksempel `.write` eller `.print`. Du kan også bruke variabler og Ruby string interpolation for å skrive variabelverdier til tekstfilen. 
 
## Dypdykk 
 
Når du skriver en tekstfil med Ruby, er det noen ting du bør være klar over. For det første vil filen bli opprettet hvis den ikke allerede eksisterer, og hvis den eksisterer vil den bli overskrevet når du bruker "w"-modus. Hvis du vil legge til data til slutten av en eksisterende fil, bør du bruke "a"-modusen i stedet. Det er også viktig å bruke `.close` metoden for å sikre at filen blir lagret og lukket når du er ferdig med å skrive til den. 
 
I tillegg bør du være forsiktig med å bruke `.puts` metoden for å skrive tall, da denne metoden automatisk legger til en linjeskift etter teksten du skriver. Hvis du vil skrive tall til filen uten et linjeskift, bør du bruke `.write` metoden i stedet. Det er også viktig å være oppmerksom på at tekstfiler er leselige for mennesker, så vær sikker på at du ikke lagrer sensitiv informasjon i en tekstfil med mindre du krypterer den først. 
 
## Se også 
* [Official Ruby Documentation for File](https://docs.ruby-lang.org/en/3.0.0/File.html)
* [10 Ruby File Examples to Save Time and Effort](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
* [How to Read and Write to Files in Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)