---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:20.745401-07:00
description: "C\uC5D0\uC11C \uAE30\uBCF8 \uC778\uC99D\uC744 \uD3EC\uD568\uD55C HTTP\
  \ \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC790\uACA9\
  \uC99D\uBA85\uC774 Base64\uB85C \uC778\uCF54\uB529\uB41C Authorization \uD5E4\uB354\
  \uB97C \uD3EC\uD568\uD558\uB294 HTTP \uC694\uCCAD\uC744 \uB9CC\uB4DC\uB294 \uAC83\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774\uB294 HTTP \uC694\uCCAD\uC5D0 \uB2E8\
  \uC21C \uC778\uC99D \uB808\uC774\uC5B4\uB97C \uCD94\uAC00\uD558\uB294 \uC77C\uBC18\
  \uC801\uC778 \uBC29\uBC95\uC73C\uB85C, \uC81C\uD55C\uB41C \uC790\uC6D0\uC5D0\u2026"
lastmod: '2024-03-13T22:44:55.922887-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C \uAE30\uBCF8 \uC778\uC99D\uC744 \uD3EC\uD568\uD55C HTTP \uC694\
  \uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC790\uACA9\uC99D\
  \uBA85\uC774 Base64\uB85C \uC778\uCF54\uB529\uB41C Authorization \uD5E4\uB354\uB97C\
  \ \uD3EC\uD568\uD558\uB294 HTTP \uC694\uCCAD\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC744\
  \ \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774\uB294 HTTP \uC694\uCCAD\uC5D0 \uB2E8\uC21C\
  \ \uC778\uC99D \uB808\uC774\uC5B4\uB97C \uCD94\uAC00\uD558\uB294 \uC77C\uBC18\uC801\
  \uC778 \uBC29\uBC95\uC73C\uB85C, \uC81C\uD55C\uB41C \uC790\uC6D0\uC5D0\u2026"
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD558\uC5EC HTTP \uC694\uCCAD\
  \ \uBCF4\uB0B4\uAE30"
weight: 45
---

## 무엇을, 왜?
C에서 기본 인증을 포함한 HTTP 요청을 보내는 것은 사용자 자격증명이 Base64로 인코딩된 Authorization 헤더를 포함하는 HTTP 요청을 만드는 것을 포함합니다. 이는 HTTP 요청에 단순 인증 레이어를 추가하는 일반적인 방법으로, 제한된 자원에 프로그래매틱하게 접근할 수 있게 합니다.

## 방법:
C에서 기본 인증을 포함한 HTTP 요청을 보내기 위해, 우리는 libcurl 라이브러리를 사용할 필요가 있습니다. 이는 인기 있는, 다재다능하며 사용하기 쉬운 클라이언트 측 URL 전송 라이브러리입니다. HTTP 및 HTTPS를 포함한 다양한 프로토콜을 처리하여 우리의 작업을 단순화합니다. 계속하기 전에 시스템에 libcurl이 설치되어 있는지 확인하십시오. 다음은 기본 인증을 포함한 GET 요청을 보내는 방법을 보여주는 기본적인 예시입니다:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // 요청이 보내지는 URL
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // 기본 인증 사용 활성화
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // 기본 인증을 위한 사용자 이름과 비밀번호 제공
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // GET 요청 수행
        res = curl_easy_perform(curl);

        // 오류 확인
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // 항상 정리
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
위의 예제에서 `"http://example.com/resource"`, `"username"`, `"password"`을 실제 URL, 사용자 이름 및 비밀번호로 교체하십시오.

이 코드는 `CURL` 개체를 초기화하고, URL을 설정하고, HTTP 기본 인증을 활성화하며, 자격증명을 지정합니다. 그런 다음 요청을 전송하고 정리합니다. 성공적으로 요청된 자원이 가져와집니다; 오류가 있으면 stderr에 출력됩니다.

성공적인 인증과 자원 접근을 가정한 샘플 출력은 프로그램이 주로 요청을 보내는 것을 보여주기 때문에 직접적으로 보여지지 않을 수 있습니다. 응답을 인쇄하기 위해, 프로그램을 확장하여 HTTP 응답 데이터를 처리할 것입니다.

## 심층 분석:
C에서 기본 인증을 포함한 HTTP 요청을 보내는 것은 robustness와 단순성을 위해 libcurl 라이브러리를 이용합니다. 역사적으로, 이러한 라이브러리 없이 순수 C에서 HTTP 요청을 만드는 것은 번거롭고 오류가 발생하기 쉬웠으며, 하위 수준의 소켓 프로그래밍과 HTTP 헤더의 수동 구성이 필요했습니다.

기본 인증 자체는 웹 초기부터 시작된 방법입니다. 단순히 디코딩할 수 있는 형식(Base64)으로 자격증명을 전송하는데, 이는 일반 텍스트 채널을 통해 본질적으로 불안전합니다. 현대 애플리케이션은 종종 OAuth 2.0이나 JWT (JSON 웹 토큰)과 같은 더 안전한 인증 방법을 선호합니다, 특히 민감한 데이터를 위해.

그러나 내부적으로, 비판적이지 않은 시스템이나, 편리함이 보안 문제보다 우선하는 빠르고 지저분한 스크립트의 경우 기본 인증이 여전히 사용됩니다. 또한, 암호화된 연결(HTTPS)과 함께 사용할 때, 그 단순성은 보다 고급 보안 메커니즘이 필요하지 않은 빠른 개발, 테스트 또는 자동화 작업에 대한 장점이 됩니다.

최첨단 보안이 불가피한 맥락에서는, 토큰 기반 인증과 같은 대안을 우선시해야 합니다. 그럼에도 불구하고, libcurl을 통해 C에서 기본 인증을 구현하는 방법을 이해하는 것은 다양한 인증 방법과 프로토콜에 적응할 수 있는 기초적인 기술을 제공하며, 보안, 편리함 및 애플리케이션 요구 사항 사이의 미묘한 절충을 반영합니다.
