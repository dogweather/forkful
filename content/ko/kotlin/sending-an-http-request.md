---
title:                "HTTP 요청 보내기"
html_title:           "Kotlin: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜 요청을 보내는지

HTTP 요청을 보내는 일은 모든 소프트웨어 개발자를 위한 필수적인 기술입니다. 이를 통해 앱과 웹 사이트가 서버와 상호작용하고 데이터를 주고받을 수 있기 때문에, HTTP 요청을 보내는 것은 애플리케이션의 중요한 부분입니다.

## 방법

```Kotlin
// 구글의 HTTP 요청 라이브러리를 사용해서 GET 요청 보내기

val url = "https://www.google.com"

// 새로운 HTTP 클라이언트 생성
val client = OkHttpClient()

// Request 객체 생성
val request = Request.Builder()
        .url(url)
        .build()

// 요청 보내기
client.newCall(request).execute().use { response ->
   if (!response.isSuccessful) throw IOException("Unexpected code $response")

   // 응답 본문 가져오기
   println(response.body?.string())
}
```

이 예시에서는 구글 홈페이지에 GET 요청을 보내고, 응답으로 받은 본문을 출력합니다.

## 깊이 파고들기

HTTP 요청을 보내는 방법은 다양하지만, 가장 일반적인 방법은 HttpURLConnection이나 OkHttp 같은 라이브러리를 사용하는 것입니다. 이들을 사용하면 필요한 Header나 Body를 추가해보거나, 다양한 HTTP 메소드를 사용해볼 수 있습니다.

## 관련 자료

[구글의 OkHttp 라이브러리](https://square.github.io/okhttp/)

[Kotlin 공식 문서 - HTTP 통신](https://kotlinlang.org/docs/networking.html)