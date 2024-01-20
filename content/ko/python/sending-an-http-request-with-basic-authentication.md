---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Python: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 기본 인증을 사용하여 요청을 보내는 것은 웹 서버에 자격 증명을 포함한 요청을 보내는 과정입니다. 이는 HTTP 통신에서 요구사항을 충족시키기 위해 서버에 접근 권한을 얻는 데 효과적입니다.

## 방법은?:

Python의 requests 라이브러리를 사용하여 HTTP 기본 인증을 포함한 요청을 보낼 수 있습니다. 다음은 이를 구현한 예제코드입니다.

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://example.com', auth=HTTPBasicAuth('user', 'pass'))
print(response.status_code)
```

이 스크립트는 성공적으로 실행되면 상태 코드 200을 출력합니다.

## 깊게 알아보기:

HTTP 기본 인증은 인터넷의 초기 시절부터 존재하던 인증 방식입니다. 그러나 오늘날에는 보안 취약점 때문에 OAuth나 토큰 기반 인증 같은 더 안전한 대안이 많이 사용됩니다.

기본 인증의 단점이 명백하더라도, 그 간단함과 초보자 친화적인 구조 때문에 여전히 부분적으로 사용되고 있습니다.

이 메소드를 이해하는 것은 인증의 기본 개념과 HTTP 프로토콜에 대한 이해를 향상시킵니다. 'requests' 라이브러리는 HTTPBasicAuth 객체를 사용하여 사용자 이름과 비밀번호를 Base64로 인코딩하고, 이를 HTTP 'Authorization' 헤더에 추가하여 요청을 보냅니다.

## 참고 자료:

1. Python 'requests' 라이브러리 공식 문서: https://docs.python-requests.org/en/latest/
2. HTTP Basic Authentication 설명: https://en.wikipedia.org/wiki/Basic_access_authentication
3. Python으로 HTTP 요청 보내기 튜토리얼: https://realpython.com/python-requests/