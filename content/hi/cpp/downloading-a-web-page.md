---
title:                "वेब पेज डाउनलोड करना"
html_title:           "C++: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Kyun

Kya aapne kabhi socha hai ki internet par aap kaise kisi bhi web page ko dekh sakte hai? Yeh kaam kaise hota hai? Yadi aap bhi iss baare mein janna chahte hai, to iss article ko padhkar aap iss process ke baare mein jaan sakte hai.

##Kaise Kare

```C++
// URL ko download karne ke liye
#include <iostream>
#include <string>
#include <curl/curl.h>

using namespace std;

// CURL library ka use karke ek data structure banaye
struct MemoryStruct {
  char *memory;
  size_t size;
};

// CURL function
static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp) {
  size_t realsize = size * nmemb;
  struct MemoryStruct *mem = (struct MemoryStruct *)userp;

// Reallocate karke memory space increase karne ke liye
  mem->memory = (char *)realloc(mem->memory, mem->size + realsize + 1);

// Naya data copy karne ke liye.
  memcpy(&(mem->memory[mem->size]), contents, realsize);
  mem->size += realsize;
  mem->memory[mem->size] = 0;

  return realsize;
}

// CURL ka use karke web page download karne ka code
int main(void) {
  CURL *curl;
  CURLcode res;

  struct MemoryStruct chunk;

  // Memory ko allocate kare
  chunk.memory = (char *)malloc(1);
  chunk.size = 0;

  curl = curl_easy_init();

  // Download karne ke liye URL set kare
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

  // Download kare
  res = curl_easy_perform(curl);

  // Result ko print kare
  cout << chunk.memory;

  // Memory free karne ke liye
  free(chunk.memory);

  // CURL close kare
  curl_easy_cleanup(curl);

  // Program ko exit kare
  return 0;
}
```

Iss coding example mein humne CURL library ka use karke ek web page ko download kiya hai. Pehle, humne ek data structure banaya jis mein hum apne download kiye huve data ko store karenge. Fir, humne CURL function banaya jis mein humne web page ko download karne ke liye URL set kiya hai. Fir, humne malloc function ka use karke memory allocate kiya hai aur free function se download kiya huva web page ka data free kar diya hai.

##Gehri Jankari

Web pages ko download karne ke liye, hume ek HTTP request bhejna hota hai. Iss request mein hum web page ka URL, method (GET, POST) aur kuch headers bhejte hai. Jab humara request server tak pahuchta hai, to server hume ek HTTP response bhejta hai jismein web page ka data hota hai. Hum CURL library ka use karke iss request aur response ko handle karte hai. CURL library humare liye server se communication ko asaan banati hai.

##Dekhe bhi

- [CURL library documentation](https://curl.se/libcurl/)
- [LearnCPP - Web file downloading using C++](https://www.learncpp.com/cpp-tutorial/a3-working-with-files-web-file-downloading/)