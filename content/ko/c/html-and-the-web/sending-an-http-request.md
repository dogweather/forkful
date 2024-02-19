---
aliases:
- /ko/c/sending-an-http-request/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:45.866292-07:00
description: "HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC6F9 \uC11C\
  \uBC84\uC5D0 \uC694\uCCAD\uC744 \uC0DD\uC131\uD558\uACE0 \uC804\uB2EC\uD558\uC5EC\
  \ \uB370\uC774\uD130\uB97C \uAC80\uC0C9\uD558\uAC70\uB098 \uC81C\uCD9C\uD558\uB294\
  \ \uACFC\uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 C\uC5B8\uC5B4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC6F9 API\uC640 \uC0C1\
  \uD638\uC791\uC6A9\uD558\uAC70\uB098, \uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\
  \uB85C\uB4DC\uD558\uAC70\uB098, \uC790\uC2E0\uB4E4\uC758 \uC5B4\uD50C\uB9AC\uCF00\
  \uC774\uC158\uC5D0\uC11C \uB2E4\uB978 \uB124\uD2B8\uC6CC\uD06C \uC11C\uBE44\uC2A4\
  \uC640 \uC9C1\uC811 \uD1B5\uC2E0\uD558\uAE30 \uC704\uD574 \uC774\u2026"
lastmod: 2024-02-18 23:09:06.946194
model: gpt-4-0125-preview
summary: "HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC6F9 \uC11C\uBC84\
  \uC5D0 \uC694\uCCAD\uC744 \uC0DD\uC131\uD558\uACE0 \uC804\uB2EC\uD558\uC5EC \uB370\
  \uC774\uD130\uB97C \uAC80\uC0C9\uD558\uAC70\uB098 \uC81C\uCD9C\uD558\uB294 \uACFC\
  \uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 C\uC5B8\uC5B4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC6F9 API\uC640 \uC0C1\uD638\
  \uC791\uC6A9\uD558\uAC70\uB098, \uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\
  \uB4DC\uD558\uAC70\uB098, \uC790\uC2E0\uB4E4\uC758 \uC5B4\uD50C\uB9AC\uCF00\uC774\
  \uC158\uC5D0\uC11C \uB2E4\uB978 \uB124\uD2B8\uC6CC\uD06C \uC11C\uBE44\uC2A4\uC640\
  \ \uC9C1\uC811 \uD1B5\uC2E0\uD558\uAE30 \uC704\uD574 \uC774\u2026"
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

HTTP 요청을 보내는 것은 웹 서버에 요청을 생성하고 전달하여 데이터를 검색하거나 제출하는 과정을 포함합니다. 프로그래머들은 C언어를 사용하여 웹 API와 상호작용하거나, 웹 페이지를 다운로드하거나, 자신들의 어플리케이션에서 다른 네트워크 서비스와 직접 통신하기 위해 이 작업을 합니다.

## 방법:

C에서 HTTP 요청을 보내려면, C가 웹 프로토콜에 대한 내장 지원을 하지 않기 때문에 주로 libcurl과 같은 라이브러리에 의존하게 됩니다. 다음은 libcurl을 사용하여 GET 요청을 수행하는 간단한 예입니다:

먼저, 시스템에 libcurl이 설치되어 있는지 확인하세요. 그런 다음 필요한 헤더를 포함시키고 소스 파일에서 libcurl 라이브러리에 링크하세요:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init(); // libcurl 핸들 초기화
    if(curl) {
        // libcurl 핸들이 받는 URL 설정
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // 데이터를 얻기 위한 콜백 정의
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL); 
        
        // 요청 수행, res는 반환 코드를 얻음
        res = curl_easy_perform(curl);
        // 오류 확인
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // 항상 정리
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

이것을 `gcc -o http_request http_request.c -lcurl`와 같은 것으로 컴파일하면, "http://example.com"으로 간단한 GET 요청을 수행합니다.

### 샘플 출력

예제는 서버의 응답을 처리하지 않으므로, 잠재적인 오류 메시지 외에는 보이는 출력을 생성하지 않습니다. 수신된 데이터를 처리하기 위한 콜백 함수의 통합은 의미 있는 상호작용을 위해 필수적입니다.

## 심화 탐구

C 프로그램에서 HTTP 요청을 보내는 개념은 언어의 강력한 네트워킹 능력과 C 자체가 빌트인 고수준 인터넷 프로토콜 지원이 없는 저수준 언어이기 때문에 외부 라이브러리와 결합되어 있습니다. 역사적으로, 프로그래머들은 libcurl과 같은 전용 라이브러리의 등장 이전에 웹 서버와 상호작용하기 위해 C에서 소켓 프로그래밍을 수동으로 사용하곤 했습니다. 이는 복잡하고 지루한 과정이었습니다.

C 위에 구축된 libcurl은 과정을 간소화하여, 소켓 프로그래밍과 HTTP 프로토콜의 세세한 부분을 추상화합니다. 이는 HTTP/HTTPS 이상의 여러 프로토콜을 지원하며, FTP, SMTP 등을 포함하여 C에서의 네트워크 프로그래밍을 위한 다재다능한 도구가 됩니다.

C에서 HTTP 요청을 위해 libcurl을 사용하는 것은 실용적이지만, 현대 프로그래밍은 종종 Python(요청 라이브러리)이나 JavaScript(Fetch API)와 같은 내장 지원이 있는 언어로 기울고 있습니다. 이러한 대안들은 직접적인 소켓 조작과 미세하게 조정된 라이브러리 사용을 통한 C에서 가능한 세밀한 제어와 성능 최적화를 희생하면서도 더 간단하고 읽기 쉬운 문법을 제공합니다.

핵심 성능 응용 프로그램이나 직접 시스템 수준 상호작용이 필요한 경우, libcurl이 웹 통신의 복잡성을 훨씬 줄여주므로 C는 여전히 실행 가능한 옵션입니다. 그러나 대부분의 고수준 웹 상호작용의 경우, 더 전문화된 웹 프로그래밍 언어를 탐색하는 것이 더 효율적일 수 있습니다.
