---
title:                "Http 요청 보내기"
html_title:           "Fish Shell: Http 요청 보내기"
simple_title:         "Http 요청 보내기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

#%% 무엇 & 왜?
HTTP 요청을 보내는 것은 네트워크를 통해 웹 서버에 데이터를 요청하는 것을 말합니다. 프로그래머들은 이를 통해 웹 애플리케이션과 상호작용하거나 데이터를 가져와서 사용할 수 있습니다.

#%% 어떻게:
```Fish Shell``` 코드 블록 안에 코딩 예제와 샘플 출력이 있는 예시를 살펴보겠습니다:

1. GET 요청 보내기:
```
curl https://www.example.com
```
2. POST 요청 보내기:
```
curl -d "username=test&password=test123" -X POST https://www.example.com/login
```

#%% 더 들어가보기:
1. 역사적 배경: HTTP 요청은 하이퍼텍스트 전송 프로토콜(HTTP)의 방식 중 하나로, 웹 개발의 발전과 함께 생겨나게 되었습니다.
2. 대안: Fish Shell을 사용하지 않는다면, Python의 requests 모듈이나 cURL 등으로도 HTTP 요청을 보낼 수 있습니다.
3. 구현 세부사항: Fish Shell은 ```curl``` 명령어를 통해 HTTP 요청을 보낼 수 있습니다.

#%% 관련 자료:
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/curl.html)
- [Python requests 모듈 공식 문서](https://docs.python-requests.org/en/latest/)
- [cURL 공식 문서](https://curl.se/docs/manpage.html)