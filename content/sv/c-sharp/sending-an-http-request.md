---
title:                "C#: Skickar en http-begäran"
simple_title:         "Skickar en http-begäran"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-anrop är en viktig del av att utveckla webbapplikationer och integrera system. Genom att använda detta protokoll kan du kommunicera med andra servrar och ta emot information som du behöver för att bygga dynamiska och interaktiva webbplatser. 

## Hur man gör

För att skicka ett HTTP-anrop i C#, behöver du först välja vilken typ av anrop du vill göra - exempelvis GET, POST, PUT, DELETE etc. Sedan måste du bygga upp en förfrågan med hjälp av rätt syntaktisk struktur. Nedan följer ett enkelt exempel med utgångspunkt från en POST-request:

```C#
// Skapa en WebRequest för din URL
WebRequest request = WebRequest.Create("https://exempel.com/api");
// Ange rätt metod - i detta fall POST
request.Method = "POST";
// Skapa en ström för att skriva ut det du vill skicka som kropp (body) av förfrågan
using (StreamWriter writer = new StreamWriter(request.GetRequestStream()))
{
    // Skriv ut dina data här
    writer.WriteLine("Namn: John");
    writer.WriteLine("Ålder: 25");
}
// Få svar från servern och läs in svaret
WebResponse response = request.GetResponse();
using (StreamReader reader = new StreamReader(response.GetResponseStream()))
{
    // Läs svaret och skriv ut det
    Console.WriteLine(reader.ReadToEnd());
}
```

Output:

```
<Status: 200>
<Meddelande: Framgångsrikt skickat anrop!>
```

## Deep Dive

Att skicka ett HTTP-anrop involverar flera steg och det är viktigt att ha en god förståelse för dem för att kunna framgångsrikt kommunicera med andra servrar. Här är några ytterligare saker som är bra att känna till när du arbetar med HTTP-anrop i C#:

1. Du kan även skicka anpassade headrar, exempelvis för att autentisera dig mot servern eller för att ange rätt innehållstyp (content-type) för din begäran.
2. För mer avancerade anrop kan det vara användbart att använda en HTTP-klient som ger dig mer kontroll över dina anrop, exempelvis HttpClient-klassen i C#.
3. Det är också viktigt att hantera fel och statuskoder på rätt sätt för att kunna hantera eventuella problem som kan uppstå i din begäran eller respons.

Se till att läsa på om HTTP-protokollet och dess specifikationer för att få en ännu djupare förståelse för hur du kan använda det i dina C#-projekt.

## Se även

- [Microsofts dokumentation om att skicka HTTP-anrop i C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.webrequest?view=net-5.0)
- [En grundläggande guide till HTTP-anrop med C#](https://www.tutorialspoint.com/csharp/csharp_sending_http_requests.htm)
- [Skillnaden mellan WebRequest och HttpClient i C#](https://www.c-sharpcorner.com/blogs/the-difference-between-httpclient-and-webrequest-using-c-sharp)