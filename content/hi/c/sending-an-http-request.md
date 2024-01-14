---
title:                "C: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

नमस्ते दोस्तों! अगर आप एक C प्रोग्रामर हैं तो आपने सुना ही होगा कि आजकल वेब डेवलपमेंट डोमेन में C का प्रयोग काफी बढ़ गया है। आपने शायद HTTP रिक्वेस्ट के बारे में सुना होगा लेकिन आपको यह पता हो तो अच्छा होगा कि यह क्या है और आप उसे कैसे भेज सकते हैं। इस ब्लॉग पोस्ट के माध्यम से हम आपको इस भाषा में बताएंगे कि आप अपने C प्रोग्रामों के अंतर्गत HTTP रिक्वेस्ट कैसे भेज सकते हैं।

## क्यों

HTTP रिक्वेस्ट तक पहुंचना एक वेब डेवलपर के लिए बहुत जरूरी है। इसके माध्यम से आप वेब सर्वर से डेटा को पढ़ और लिख सकते हैं। आप यह जान सकते हैं कि किसी वेब पेज को लोड करने के लिए आप कैसे सर्वर से डेटा का अनुरोध करते हैं। 

## कैसे

आइए अब देखते हैं कि कैसे आप C में HTTP रिक्वेस्ट भेज सकते हैं। आप नीचे दिए गए कोड ब्लॉक में दिए गए कोड को देख सकते हैं। यह एक बेसिक HTTP रिक्वेस्ट है जो आप अपने प्रोग्राम में इस्तेमाल कर सकते हैं। आप अपनी आवश्यकताओं के अनुसार कोड में परिवर्तन कर सकते हैं।

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int main() {
  // HTTP Request message
  char *message = "GET / HTTP/1.1\r\n\r\n";
  
  // Server address
  char *server_address = "www.example.com";
  
  // Create socket
  int socket_desc = socket(AF_INET, SOCK_STREAM, 0);
  
  // Check if socket was created successfully
  if (socket_desc == -1) {
    printf("Error creating socket!");
    return 1;
  }
  
  // Get server info
  struct hostent *server = gethostbyname(server_address);
  if (server == NULL) {
    printf("Error getting server by name!");
    return 1;
  }
  
  // Create server address