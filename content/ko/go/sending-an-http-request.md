---
title:                "HTTP 요청 보내기"
html_title:           "Go: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 무슨 & 왜?

HTTP 요청을 보내는 것은 서버에게 데이터를 요청하거나 전송하기 위해 사용되는 것입니다. 프로그래머들은 이를 통해 웹 애플리케이션을 만들거나 데이터를 가져와서 사용할 수 있습니다.

# 어떻게:

```Go 
resp, err := http.Get("https://www.example.com")
``` 

위의 코드는 Go 언어에서의 기본적인 HTTP 요청 방법을 보여줍니다. 이를 통해 "www.example.com"에서 데이터를 가져오는 요청을 보내고, 서버로부터의 응답과 오류를 처리할 수 있습니다.

```Go
fmt.Println(resp.StatusCode) // 200
fmt.Println(resp.Header) // map[Content-Type:[text/html; charset=utf-8] Date:[Wed, 18 Nov 2020 00:00:00 GMT]]
```

위의 코드는 서버로부터의 응답에서 상태 코드와 헤더 정보를 출력하는 방법을 보여줍니다. 이를 통해 프로그래머들은 서버로부터의 응답을 받고, 그에 따라 적절한 로직을 수행할 수 있게 됩니다.

# 자세히 들어가기:

HTTP 요청은 웹의 발전과 함께 등장한 개념입니다. 웹 애플리케이션을 개발하거나 데이터를 가져오려면 서버에게 요청을 보내야만 합니다. 이를 위해 여러 가지 언어와 프레임워크에서 HTTP 요청을 보내는 방법들을 제공하고 있으며, Go 언어에서는 기본적으로 제공하는 라이브러리를 사용하여 간단하게 요청을 보낼 수 있습니다.

또한, HTTP 요청 외에도 웹 소켓과 같은 다른 방식의 통신도 존재합니다. 하지만 많은 경우에 HTTP 요청으로 충분하고 간편하게 데이터를 주고받을 수 있습니다. 

어떤 언어와 프레임워크를 사용하더라도 HTTP 요청 방식은 기본적으로 비슷합니다. 위에서 소개한 Go 언어의 ```http.Get()``` 함수 대신에 다른 언어에서는 ```GET``` 메서드를 사용하는 방법이 일반적입니다. 또한, 서버로 데이터를 보내는 경우에는 ```POST``` 메서드를 주로 사용합니다.

# 관련 자료:

- [더 자세한 HTTP 요청 예제](https://golang.org/pkg/net/http/#Request)
- [웹 개발을 위한 Go 언어 공식 문서](https://golang.org/doc/)
- [HTTP 요청에 대한 기본적인 개념 설명](https://www.cloudflare.com/learning/ddos/glossary/http-request/)