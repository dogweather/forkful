---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
웹 페이지 다운로드는 특정 웹 페이지의 데이터를 로컬 컴퓨터에 저장하는 것입니다. 이는 웹 스크래핑, 데이터 마이닝 또는 오프라인 브라우징 등을 위해 프로그래머들이 사용합니다.

## 만들기:
이 단순한 프로그램은 libcurl 라이브러리를 사용하여 웹 페이지를 다운로드합니다. 이 예제에서는 google.com 페이지를 다운로드합니다.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
	CURL *curl;
	CURLcode res;

	curl_global_init(CURL_GLOBAL_DEFAULT);
	curl = curl_easy_init();

	if(curl) {
		curl_easy_setopt(curl, CURLOPT_URL, "https://www.google.com");

		/* 요청 수행 */
		res = curl_easy_perform(curl);
		/* 오류 확인 */
		if(res != CURLE_OK)
			fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

		/* 마지막으로 curl 사용 정리 */
		curl_easy_cleanup(curl);
	}
	curl_global_cleanup();

	return 0;
}
```
컴파일 및 실행 결과, 프로그램은 google.com의 소스 코드를 출력합니다.

## 깊이 이해하기:
웹 페이지 다운로드는 월드 와이드 웹의 초기 시절부터 있었습니다. 이 기술은 데이터를 분석하거나, 웹 페이지의 사본을 저장하거나, 인터넷 연결 없이 웹 페이지를 브라우징하려는 등 다양한 이유로 사용되었습니다.

다른 언어 또는 도구를 사용하여 웹 페이지를 다운로드할 수도 있습니다. 파이썬의 requests 라이브러리나 자바의 HttpUrlConnection과 같은 것들이 있습니다. C에서는 libcurl이 가장 대중적인 라이브러리입니다.

libcurl은 다양한 인터넷 프로토콜 지원하며 쿠키 처리, 인증, 프록시 등의 기능을 제공합니다. 이 예에서는 가장 기본적인 GET 요청을 보여줍니다.

## 참고 링크:
- libcurl 공식 문서: https://curl.haxx.se/libcurl/c/
- 웹 스크래핑에 대한 위키백과 기사: https://ko.wikipedia.org/wiki/웹_스크레이핑
- C 프로그래밍에 대한 정보: https://ko.wikipedia.org/wiki/C_(프로그래밍_언어)