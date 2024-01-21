---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:01:36.588487-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
HTTP 요청에 기본 인증(Basic Authentication)을 추가하는 것은, 서버에 사용자 이름과 비밀번호를 전송해 접근 권한을 확인하는 과정입니다. 이를 통해 개발자들은 보안이 필요한 데이터에 안전하게 접근할 수 있습니다.

## How to (방법):
```Go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, _ := http.NewRequest("GET", "http://yourapi.com/data", nil)
	
	// Basic Authentication 설정
	username := "user"
	password := "pass"
	encodedCredentials := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
	req.Header.Add("Authorization", "Basic " + encodedCredentials)

	resp, _ := client.Do(req)
	fmt.Println("Status Code:", resp.StatusCode)
}
```

Sample output:

```
Status Code: 200
```

## Deep Dive (심화 학습):
Basic Authentication은 HTTP/1.0 으로부터 지원되기 시작했습니다. 웹 통신에서 가장 오래된 보안 방식 중 하나입니다. 하지만, 요청이 유선상에서 평문으로 전송되기 때문에 HTTPS 위에서만 사용되어야 합니다. 기본 인증 대신 OAuth 등 다른 방식이 종종 사용되기도 합니다. Golang에서는 http 패키지를 사용해 이 과정을 간단히 구현할 수 있으며, `base64` 표준 라이브러리로 인증 헤더를 인코딩합니다.

## See Also (참고 자료):
- Go 언어의 공식 http 패키지 문서: https://pkg.go.dev/net/http
- Basic Authentication 소개: https://datatracker.ietf.org/doc/html/rfc7617
- 보안 연결 (HTTPS) 소개: https://datatracker.ietf.org/doc/html/rfc2818
- 보다 안전한 인증 방법 OAuth 2.0: https://oauth.net/2/