---
title:                "웹 페이지 다운로드시키기"
html_title:           "C++: 웹 페이지 다운로드시키기"
simple_title:         "웹 페이지 다운로드시키기"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜

웹 페이지 다운로드를 하는 이유는 다양합니다. 예를 들어, 소스 코드를 분석하거나 웹 크롤링을 통해 데이터를 수집하려는 경우가 있을 수 있습니다.

## 어떻게

웹 페이지를 다운로드하기 위해서는 다음과 같은 단계를 따르면 됩니다.

```C++
#include <iostream>
#include <curl/curl.h>
using namespace std;

// 웹 페이지 다운로드를 위한 함수
size_t WriteCallback(char *buf, size_t size, size_t nmemb, void *userp) {
  // 받은 데이터를 파일로 저장
  size_t written = fwrite(buf, size, nmemb, (FILE *)userp);
  return written;
}

// main 함수
int main() {
  // 다운로드할 웹 페이지의 주소
  string url = "https://www.example.com";
  // 저장할 파일의 경로와 이름
  string filename = "downloaded.html";

  // curl 핸들 초기화
  CURL *curl;
  FILE *file;

  // curl 핸들 생성
  curl = curl_easy_init();

  if (curl) {
    // 파일을 쓰기 모드로 열기
    file = fopen(filename.c_str(), "wb");

    // curl 옵션 설정
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, file);

    // 웹 페이지 다운로드
    CURLcode res = curl_easy_perform(curl);

    // 다운로드 성공 시 0 반환
    if (res == 0) {
      cout << "Web page downloaded successfully!" << endl;
    }
    else {
      cout << "Failed to download web page." << endl;
    }

    // 핸들 해제
    curl_easy_cleanup(curl);
    // 파일 닫기
    fclose(file);
  }

  return 0;
}
```

위 코드를 실행하면, "sample.html" 파일이 생성되고 해당 웹 페이지가 다운로드됩니다.

## 딥 다이브

웹 페이지를 다운로드하기 위해서는 HTTP 요청을 보내고 해당 응답을 받아야 합니다. 위 예제에서는 cURL 라이브러리를 사용하여 해당 작업을 수행했습니다. 또한, 다운로드된 파일의 경로와 다운로드에 실패한 경우 어떻게 처리할지에 대한 추가적인 로직도 포함해야 합니다.

## See Also

- [libcurl 사용법](https://curl.se/libcurl/c/CURLOPT_WRITEFUNCTION.html) 
- [웹 크롤링 기초](https://www.freecodecamp.org/news/web-scraping-and-crawling-101-with-python/)