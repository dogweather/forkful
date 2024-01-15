---
title:                "웹 페이지 다운로드하기"
html_title:           "C: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜 

웹 페이지를 다운로드하는 이유는, 그 페이지에 대한 정보를 얻기 위해서입니다. 여러분이 어떤 웹 페이지에 대해 궁금한 정보가 있다면, 그 페이지를 다운로드해서 즉시 접근할 수 있습니다. 

## 방법

다음은 C 언어를 사용하여 컴퓨터에서 웹 페이지를 다운로드하는 간단한 예제입니다. 

```
#include <stdio.h>
#include <stdlib.h>

int main() {

    // 라이브러리를 포함합니다.
    #include <curl/curl.h>

    // 다운로드 받을 URL 주소를 지정합니다.
    CURL * curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.google.com");

    // 다운로드한 데이터를 저장할 파일을 생성합니다.
    FILE* fp = fopen("downloaded_page.html", "wb");
    if (!fp) {
        return 1;
    }

    // 다운로드를 수행하고 저장합니다.
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    CURLcode result = curl_easy_perform(curl);

    // 열었던 파일 포인터를 닫습니다.
    fclose(fp);

    if (result == CURLE_OK) {
        printf("웹 페이지를 다운로드했습니다!");
    } else {
        printf("웹 페이지를 다운로드하는 데 실패했습니다.");
    }

    // 라이브러리를 해제합니다.
    curl_easy_cleanup(curl);

    return 0;
}
```

위의 예제를 실행하면, 현재 디렉토리에 `downloaded_page.html` 파일이 생성되고 해당 파일 안에는 구글 홈페이지의 소스 코드가 저장됩니다. 

## 더 깊이 알아보기

다운로드한 웹 페이지의 소스 코드를 파싱하고 원하는 정보를 추출하는 등의 작업도 가능합니다. 또한, 웹 페이지를 다운로드하는 방법에는 `libcurl` 외에도 다른 방법들도 존재합니다. 한번 시도해보세요!

## 또 다른 자료

- [C로 구현한 웹 크롤러 예제](https://github.com/sleepdefic1ency/c-webcrawler)
- [libcurl 공식 문서](https://curl.haxx.se/libcurl/)