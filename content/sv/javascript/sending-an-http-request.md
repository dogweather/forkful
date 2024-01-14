---
title:                "Javascript: Skicka en HTTP-begäran."
simple_title:         "Skicka en HTTP-begäran."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-request är en viktig del av webbutveckling. Det låter dig kommunicera med andra servrar och hämta information från internet. Det är det sätt på vilket webbsidor fungerar och gör det möjligt för oss att få tillgång till olika resurser på nätet.

## Så här gör du

För att skicka en HTTP-request i Javascript kan du använda dig av det inbyggda `XMLHttpRequest` objektet. Här är ett exempel på hur du kan göra det:

```Javascript
// Skapa ett nytt XMLHttpRequest objekt
var request = new XMLHttpRequest();

// Ange vilken URL du vill skicka requesten till
var url = "https://exempel.api.com/users";

// Ange vilken typ av request du vill göra, i detta fall GET
request.open('GET', url);

// Skicka requesten
request.send();

// Lyssna på när requesten är klar och gör något med svaret
request.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    // Här kan du göra något med svaret från servern
    console.log(request.responseText);
  }
};
```

I detta exempel skapar vi först ett nytt `XMLHttpRequest` objekt och anger sedan vilken url vi vill skicka requesten till. Därefter öppnar vi requesten med `open()` funktionen och skickar den med `send()` funktionen. Slutligen, när requesten är klar och servern har svarat, så behandlar vi svaret med hjälp av `onreadystatechange` funktionen.

## Djupdykning

När vi skickar en HTTP-request, så är det viktigt att förstå de olika delarna av requesten. Det finns flera olika HTTP-metoder som man kan använda för att göra requesten, men de vanligaste är **GET** och **POST**.

**GET** används för att hämta data från en server, medan **POST** används för att skicka data till en server. Det finns också andra metoder som kan användas för olika ändamål, som **PUT** för att uppdatera data och **DELETE** för att ta bort data från en server.

Det är också viktigt att känna till att en HTTP-request består av olika delar, såsom URL, metoden som används, eventuella headers och payload (data som skickas med requesten). Genom att förstå dessa delar kan man bygga mer avancerade och effektiva HTTP-request.

## Se även

- [Javscript - HTTP-request](https://www.w3schools.com/js/js_ajax_http.asp)
- [HTTP-request - Wikipedia](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods)