---
title:                "웹 페이지 다운로드"
html_title:           "C: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇? & 왜?

웹 페이지를 다운로드하는 것은 웹 상에서 특정 웹 페이지를 로컬 컴퓨터로 가져오는 프로세스입니다. 프로그래머들은 이것을 왜 할까요? 주로 웹 페이지의 데이터를 스크래핑하기 위해 사용됩니다. 이 데이터는 후속 분석 또는 다른 프로그램에서 사용될 수 있습니다.

## 어떻게?

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    res = curl_easy_perform(curl);
    
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    
    curl_easy_cleanup(curl);
  }
  
  return 0;
}
```

위의 코드는 libcurl(C로 작성된 무료 오픈 소스 라이브러리)를 사용하여 웹 페이지를 다운로드하는 예제입니다. 해당 라이브러리를 사용하기 전에는 컴파일러 명령어에 "-lcurl"을 추가해야 합니다. 위의 코드는 예외 처리 등의 추가 작업이 필요하지 않은 기본적인 예제입니다.

## 깊게 들어가기

1997년에 처음 나오게 된 libcurl은 URL(URL 표준화 작업을 담당하는 조직)과 연관되어 있습니다. 이 라이브러리는 파일 다운로드 뿐만 아니라 http, https, ftp, ftps, file 등 다양한 프로토콜을 지원합니다. libcurl을 대체할 수 있는 다른 라이브러리로는 wget이 있습니다.

위의 예제에서 사용된 curl_easy_setopt 함수를 통해 여러 옵션을 설정할 수 있습니다. 예를 들어, "CURLOPT_FOLLOWLOCATION" 옵션을 설정하면 서버에서의 리디렉션을 자동으로 따라갑니다.

## 참고

- [libcurl 공식 사이트](https://curl.haxx.se/libcurl/)
- [libcurl을 사용한 간단한 웹 스크래핑 예제](https://stackoverflow.com/questions/13265303/minimal-c-example-for-downloading-page-programmatically)