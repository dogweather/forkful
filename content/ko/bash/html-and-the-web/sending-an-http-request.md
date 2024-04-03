---
date: 2024-01-20 17:59:27.739415-07:00
description: "HTTP \uC694\uCCAD\uC740 \uC6F9 \uC11C\uBC84\uC5D0 \uC815\uBCF4\uB97C\
  \ \uC694\uCCAD\uD558\uAC70\uB098 \uC804\uC1A1\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\uC774\uD130\uB97C \uAC80\uC0C9\
  \uD558\uACE0, \uC6F9 API\uC640 \uC0C1\uD638 \uC791\uC6A9\uD558\uBA70, \uB2E4\uC591\
  \uD55C \uC11C\uBE44\uC2A4\uC640 \uC5F0\uB3D9\uD558\uAE30 \uC704\uD574 \uC774\uB97C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.477581-06:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC740 \uC6F9 \uC11C\uBC84\uC5D0 \uC815\uBCF4\uB97C \uC694\
  \uCCAD\uD558\uAC70\uB098 \uC804\uC1A1\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4\
  ."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## What & Why? (무엇과 왜?)

HTTP 요청은 웹 서버에 정보를 요청하거나 전송하는 방법입니다. 프로그래머는 데이터를 검색하고, 웹 API와 상호 작용하며, 다양한 서비스와 연동하기 위해 이를 사용합니다.

## How to: (방법)

Bash에서는 `curl`이라는 명령어로 HTTP 요청을 보냅니다. 간단한 GET 요청부터 시작해보겠습니다:

```Bash
curl http://example.com
```

출력:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

POST 요청으로 데이터를 보내려면 다음과 같이 할 수 있습니다:

```Bash
curl -d "param1=value1&param2=value2" -X POST http://example.com/post
```

데이터를 JSON 형식으로 보내려면:

```Bash
curl -H "Content-Type: application/json" -d '{"key1":"value1", "key2":"value2"}' http://example.com/post
```

## Deep Dive (심층 탐구)

과거에는 웹 서버에 요청을 보내기 위해 복잡한 소켓 프로그래밍이 필요했습니다. `curl`은 1997년 Daniel Stenberg에 의해 개발되었으며, 커맨드 라인에서 간단하게 웹 통신를 가능하게 했습니다.

대안으로는 `wget`이 있지만, 주로 파일을 다운로드하는데 쓰입니다. `curl`은 좀 더 다양한 프로토콜과 요청 타입을 지원합니다.

HTTPRequest하는 방법으로 패키지 관리 시스템이나, 가상 환경 도구에서 사용되는 내장 함수들이 있습니다만, bash에서는 `curl`과 같은 명령어가 가장 직관적이고 간단합니다.

쉘 스크립트에서 HTTP 요청을 보낼 때에는 응답 체크, 에러 핸들링, 응답 데이터 파싱 등 추가적인 구현이 필요할 수 있습니다.

## See Also (추가 정보)

- `curl` 공식 문서: [https://curl.se/docs/](https://curl.se/docs/)
- HTTP 요청에 대한 자세한 설명: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- `wget` vs `curl`: [https://daniel.haxx.se/docs/curl-vs-wget.html](https://daniel.haxx.se/docs/curl-vs-wget.html)
