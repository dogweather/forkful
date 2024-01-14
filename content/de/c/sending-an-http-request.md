---
title:                "C: Senden einer http Anfrage"
simple_title:         "Senden einer http Anfrage"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

HTTP-Anfragen sind ein wichtiger Bestandteil der modernen Internetkommunikation. Sie ermöglichen es uns, Daten zwischen einem Client und einem Server auszutauschen. Durch das Senden von HTTP-Anfragen können wir beispielsweise auf Webseiten zugreifen oder Daten von einer API erhalten. In diesem Blog-Beitrag erfährst du, wie du mit C-Code HTTP-Anfragen senden kannst.

## Wie man es macht

Das Senden einer HTTP-Anfrage in C erfordert die Verwendung von Socket-Programmierung. Hier ist ein Beispiel, das eine GET-Anfrage an eine URL sendet und die Serverantwort ausgibt:

```C
#include <stdio.h> 
#include <string.h> 
#include <sys/socket.h> 
#include <arpa/inet.h> 

int main(int argc, char *argv[]) 
{
	int socket_desc;
	struct sockaddr_in server;
	char *message, server_reply[10000];

	// Socket erstellen
	socket_desc = socket(AF_INET, SOCK_STREAM, 0);
	if (socket_desc == -1) {
		printf("Konnte keinen Socket erstellen");
	}
	
	// Server Konfiguration
	server.sin_addr.s_addr = inet_addr("www.beispiel.com"); // Anpassen an gewünschte URL
	server.sin_family = AF_INET;
	server.sin_port = htons(80);
	
	// Verbindung aufbauen
	if (connect(socket_desc, (struct sockaddr *)&server, sizeof(server)) < 0) {
		puts("Verbindung fehlgeschlagen");
		return 1;
	}
	puts("Verbindung erfolgreich hergestellt");

	// HTTP-Anfrage erstellen
	message = "GET / HTTP/1.1\r\nHost: www.beispiel.com\r\nConnection: close\r\n\r\n"; // Anpassen an gewünschte URL
	if (send(socket_desc, message, strlen(message), 0) < 0) {
		puts("Senden fehlgeschlagen");
		return 1;
	}
	puts("HTTP-Anfrage gesendet\n");
	
	// Serverantwort empfangen
	if (recv(socket_desc, server_reply, 10000, 0) < 0) {
		puts("Empfangen fehlgeschlagen");
		return 1;
	}
	puts("Serverantwort empfangen\n");
	puts(server_reply);
	
	// Socket schließen
	close(socket_desc);
	
	return 0;
}
```

Die Ausgabe dieses Beispiels sollte die HTML-Seite der angegebenen URL enthalten. Beachte, dass die HTTP-Anfrage und die Serverantwort als Strings behandelt werden müssen.

## Tiefer eintauchen

Um eine HTTP-Anfrage in C zu senden, müssen wir verstehen, wie die HTTP-Protokolle funktionieren. GET ist die einfachste Form der HTTP-Anfrage, die verwendet wird, um Daten von einem Server abzurufen. Die Anfrage besteht aus einer Request-Linie, Header-Zeilen und einer leeren Zeile, gefolgt von optionalen Nachrichten-Body-Daten.

Um eine vollständige HTTP-Anfrage aufzubauen, müssen wir die Anfrage-Methode und die Adresse der angeforderten Ressource angeben. Zusätzlich können wir Header-Zeilen hinzufügen, um zusätzliche Informationen wie Benutzer-Agent oder Cookies zu übermitteln. Es ist auch wichtig, die Verbindung mit dem Server nach dem Senden der Anfrage zu schließen, indem wir "Connection: close" in den Header-Zeilen angeben.

Für eine tiefere Auseinandersetzung mit der HTTP-Protokollarchitektur und den verfügbaren Anfragemethoden empfehle ich, sich mit den offiziellen Spezifikationen vertraut zu machen.

## Siehe auch

- [HTTP Request Methods (Übersicht)](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [HTTP/1.1 Specifikation](https://tools.ietf.org/html/rfc2616)
- [Beispielprogramme für Socket-Programmierung in C](https://www.geeksforgeeks.org/socket-programming-cc/)