---
title:                "Eine Webseite herunterladen"
html_title:           "C++: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Du möchtest wahrscheinlich wissen, warum es wichtig ist, eine Webseite zu downloaden. Wenn du z.B. eine Webseite offline durchsuchen möchtest, ohne ständig eine Internetverbindung zu haben, dann ist das Downloaden einer Webseite die beste Option.

## Wie geht es

Das Downloaden einer Webseite mit C++ ist relativ einfach. Zunächst benötigst du eine Bibliothek, die das Herunterladen von Webseiten unterstützt. Eine beliebte Option ist cURL. Hier ist ein Beispielcode, wie du mit cURL eine Webseite in C++ herunterladen kannst:

```C++
#include <iostream>
#include <curl/curl.h> 
using namespace std;
 
int main(){
  //Initialisierung von cURL
	CURL *curl;
	CURLcode res;
	
	//URL der zu downloadenden Webseite
	string url = "https://www.example.com"; 
	
	//Initilaerung von cURL
	curl = curl_easy_init();
	if(curl) {
		//Setzen der URL
		curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
		
		//Setzen des HTTP Protokolls
		curl_easy_setopt(curl, CURLOPT_PROTOCOLS, CURLPROTO_HTTPS);
		
		//Herunterladen der Webseite
		res = curl_easy_perform(curl);
		if(res!=CURLE_OK) {
			//Fehlerbehandlung
			cerr << "Fehler: " << curl_easy_strerror(res) << endl;
		}
		//Aufräumen
		curl_easy_cleanup(curl);
 	}
	
	return 0;
}
```

Der obige Code verwendet cURL, um eine HTTPS-Webseite herunterzuladen. Du kannst auch HTTP oder andere Protokolle verwenden, je nachdem, welches Protokoll die Webseite unterstützt. Nach dem Herunterladen der Webseite kannst du die Ausgabe wie folgt erhalten:

```
<html>
<head>
  <title>Beispiel Webseite</title>
</head>
<body>
  <h1>Willkommen!</h1>
  <p>Dies ist eine Beispiel Webseite.</p>
</body>
</html>
```

## Deep Dive

Wenn du tiefer in das Herunterladen von Webseiten mit C++ eintauchen möchtest, kannst du auch die verschiedenen cURL-Optionen erforschen, um eine optimale Konfiguration zu finden. Du kannst auch andere Bibliotheken wie libcurl oder Boost C++ Libraries verwenden.

## Siehe auch

- [cURL Webseite](https://curl.haxx.se/)
- [libcurl Webseite](https://curl.haxx.se/libcurl/)
- [Boost C++ Libraries Webseite](https://www.boost.org/)