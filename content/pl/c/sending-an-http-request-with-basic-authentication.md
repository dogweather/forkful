---
title:                "C: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele aplikacji i stron internetowych korzysta z uwierzytelniania podstawowego (basic authentication), aby zapewnić bezpieczeństwo i dostęp tylko dla uprawnionych użytkowników. Dlatego warto poznać, jak wysyłać żądanie HTTP z uwierzytelnieniem podstawowym, aby móc swobodnie korzystać z różnych serwisów.

## Jak to zrobić

Aby wysłać żądanie HTTP z uwierzytelnieniem podstawowym w języku C, najpierw musimy utworzyć odpowiednie nagłówki (headers) dla naszego żądania. Dzięki nim serwer będzie wiedział, że wykorzystujemy uwierzytelnianie podstawowe i będzie wymagał od nas podania loginu i hasła.

Następnie musimy zaszyfrować login i hasło w formacie Base64, aby były bezpiecznie przesłane w zapytaniu. Możemy to zrobić używając funkcji `base64_encode` lub wykorzystując gotową bibliotekę, np. OpenSSL.

W skrypcie poniżej pokazane jest, jak wysłać żądanie GET z uwierzytelnieniem podstawowym do serwera zabezpieczonego hasłem:

```C
#include <stdio.h>
#include <curl/curl.h>
#include <openssl/bio.h>
#include <openssl/evp.h>

// Funkcja kodująca login i hasło w formacie Base64
char *base64_encode(const unsigned char *input, int length)
{
  BIO *bmem, *b64;
  BUF_MEM *bptr;
  b64 = BIO_new(BIO_f_base64());
  bmem = BIO_new(BIO_s_mem());
  b64 = BIO_push(b64, bmem);
  BIO_write(b64, input, length);

  BIO_flush(b64);
  BIO_get_mem_ptr(b64, &bptr);

  char *buff = (char *)malloc(bptr->length);
  memcpy(buff, bptr->data, bptr->length-1);
  buff[bptr->length-1] = 0;

  // Usuń znaki nowej linii i zamień spacje na znaki "+"
  for (int i = 0; i < strlen(buff); i++) {
    if(buff[i] == '\n') {
      buff[i] = '\0';
    }
    else if(buff[i] == ' ') {
      buff[i] = '+';
    }
  }

  BIO_free_all(b64);
  return buff;
}

// Funkcja obsługująca zdarzenia związane z przesyłaniem danych przez CURL
size_t write_callback(char *ptr, size_t size, size_t nmemb, void *userdata)
{
    // Przekaż dane do standardowego strumienia wyjścia
    return fwrite(ptr, size, nmemb, stdout);
}

int main(void)
{
  CURL *curl;
  CURLcode res;
  char *encoded_credentials;

  // Utwórz nagłówki z informacją o uwierzytelnieniu podstawowym
  struct curl_slist *headers = NULL;
  headers = curl_slist_append(headers, "Authorization: Basic");

  // Zaszyfruj login i hasło w formacie Base64
  encoded_credentials = base64_encode("user:password", strlen("user:password"));

  // Ustaw nagłówek z zaszyfrowanymi danymi
  headers = curl_slist_append(headers, encoded_credentials);

  // Inicjalizacja CURL
  curl = curl_easy_init();
  if(curl) {
    // Ustaw opcje dla żądania GET
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/secure");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);

    // Wyślij żądanie i odbierz odpowiedź
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    // Zakończ połączenie CURL
    curl_easy_cleanup(curl);
  }

  // Zwolnij pamięć zaalokowaną dla kodowania Base64