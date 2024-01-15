---
title:                "웹 페이지 다운로드"
html_title:           "Kotlin: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜 웹 페이지 다운로드를 하는가?

웹 페이지를 다운로드하는 이유는 간단합니다. 대부분의 사람들은 인터넷을 사용하여 정보를 찾거나 제품을 구매하기 위해 웹 페이지를 방문합니다. 로컬 컴퓨터에 웹 페이지를 다운로드하고 나중에 오프라인으로 확인하면 더 편리합니다.

## 다운로드하는 방법

```Kotlin
// Ktor 의존성을 추가합니다.
dependencies {
    implementation "io.ktor:ktor-client-core:$ktor_version"
    implementation "io.ktor:ktor-client-features:$ktor_version"
    implementation "io.ktor:ktor-client-okhttp:$ktor_version"
}
```

```Kotlin
// Ktor 클라이언트를 초기화합니다.
val client = HttpClient(OkHttp) {
    install(HttpTimeout) {
        requestTimeoutMillis = 5000L
    }
}
```

```Kotlin
// 웹 페이지 다운로드 함수를 만듭니다.
suspend fun downloadWebPage(url: String): String {
    val response = client.get<String>(url)
    return response
}
```

```Kotlin
// 다운로드 함수를 호출하고 결과를 출력합니다.
println(downloadWebPage("https://kotlinlang.org/"))
```

### 출력:

```
<!DOCTYPE html>
<html>
<head>
    <title>Kotlin Programming Language</title>
    ...
```

## 깊이 파고들기

- 웹 서버와의 통신에서 Timeout 설정을 적용하여 응답 시간을 제어할 수 있습니다.
- 웹 페이지 다운로드 외에도, Ktor 라이브러리를 사용하여 웹 소켓 통신, REST API 호출 등 다양한 기능을 구현할 수 있습니다.

## 더 알아보기

- [Ktor 공식 문서](https://ktor.io/)
- [Kotlin 공식 사이트](https://kotlinlang.org/)
- [Kotlin 샘플 코드](https://github.com/Kotlin/kotlin-compiler-samples)