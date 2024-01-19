---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청은 원격 서버에 정보를 요청하는 방법입니다. 프로그래머는 이를 통해 웹 응용 프로그램에서 데이터를 획득하거나 작업을 수행합니다.

## 사용 방법:

```bash
# cURL을 사용한 간단한 GET 요청
curl http://example.com

# 응답을 파일로 저장
curl http://example.com -o example.txt

# POST 요청의 예
curl -d "param1=value1&param2=value2" -X POST http://example.com
```
간단한 출력 예:
```bash
<!doctype html>
...
</html>
```

## 깊게 알아보기:

그래도 Bash에서 HTTP 요청을 보내는 것은 상대적으로 최근의 현상입니다. 이는 웹과 클라우드 기반 테크놀로지가 주류가 되면서 본격적으로 인기를 얻었습니다.

대안으로 Python의 requests나 JavaScript의 axios 같은 다른 언어의 도구를 사용할 수 있습니다.

cURL은 libcurl 라이브러리를 이용하여 HTTP 통신을 수행합니다. HTTP 요청은 TCP/IP 소켓을 이용하며, 요청 헤더와 본문으로 구성됩니다.

## 참고 자료:

curl 공식 문서: https://curl.haxx.se/docs/manpage.html
Bash Programming Guide: http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html