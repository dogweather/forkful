---
title:                "Kotlin: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Kyun
Kotlin ek prasiddh programming bhasha hai jo aaj kal bahut se developers ke liye pasand ki jaati hai. Iska upyog kayi tarah ke application development me kiya ja sakta hai, jaise Android aur server-side development. Is bhasha ke aate hi hum english jaise statically typed, object-oriented features aur functional programming ka maza le sakte hai. Iss article me hum Kotlin ke istemal se web pages ko download karna sikhenge. Yeh ek prakriya hai jiske kaaran hum kisi bhi website ko apne local machine me aaram se access kar sakte hai.

## Kaise Kare
Kotlin me ek HTTP client library "OkHttp" hai jo web requests ke liye kaafi famous hai. Hum is library ka upyog karke bahut aasaani se web pages ko download kar sakte hai. Yaha iss article me Kotlin 1.5.10 aur OkHttp version 4.9.1 ka upyog kiya gaya hai.

```Kotlin
val client = OkHttpClient()
val request = Request.Builder()
  .url("https://www.example.com")
  .get()
  .build()

val response = client.newCall(request).execute()
val responseBody = response.body()?.string()
```
Iss code me hum client object ko banate hai, url aur request type ko specify karte hai, aur phir uss request ko execute karke response body ko retrieve karte hai.

Ab iss response body ko kisi file me store kar sakte hai aur phir usse local machine me open kar sakte hai.

## Gahraai 
Web page download karna ek important process hai aur usse karne ke liye kayi baar hume authentication, redirects aur cookies ka bhi bahut dhyaan dena padta hai. Isliye, apne code me hume in features ka bhi dhyaan rakhna chahiye.

## Dekhe Bhi
- [OkHttp Github page](https://github.com/square/okhttp)
- [Kotlin official website](https://kotlinlang.org/)
- [Kotlin Tutorials on Youtube](https://www.youtube.com/playlist?list=PLQkwcJG4YTCSbdIxcRSY0nRUoSjLpI1D1)

Dhanyavaad!