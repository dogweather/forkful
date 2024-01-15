---
title:                "기본 인증으로 http 요청 보내는 방법"
html_title:           "Swift: 기본 인증으로 http 요청 보내는 방법"
simple_title:         "기본 인증으로 http 요청 보내는 방법"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 것이 중요한 이유는 서비스 또는 애플리케이션의 보안을 위해 사용자의 자격 증명을 확인해야 할 때입니다. 이를 통해 안전하지 않은 정보에 접근하는 것을 방지할 수 있습니다.

## 진행 방법

이제 본격적으로 Swift에서 HTTP 요청에 기본 인증을 추가하는 방법을 알아보겠습니다. 먼저 사용해야 할 라이브러리는 'Alamofire'입니다. Alamofire는 Swift에서 편리하게 HTTP 요청을 보낼 수 있도록 도와주는 라이브러리입니다.

```Swift
import Alamofire
```

이제 'Alamofire.request'를 사용하여 HTTP 요청을 보냅니다. 이때, 인증에 필요한 매개변수를 추가해주어야 합니다.

```Swift
Alamofire.request("https://example.com", method: .get, parameters: nil, encoding: JSONEncoding.default, headers: ["Authorization": "Basic username:password"]).responseJSON { response in
    print(response)
}
```

위의 코드에서 'headers' 매개변수에 'Basic' 인증을 사용하고, 인증할 사용자 아이디와 비밀번호를 입력하면 됩니다. 이제 서버로부터 받은 응답을 확인할 수 있습니다.

## 더 깊게 파헤치기

위의 예제에서는 HTTP GET 메소드를 사용했지만, POST, PUT, DELETE 등 다른 메소드를 사용하여 요청을 보낼 수도 있습니다. 그리고 인증 정보를 직접 입력하는 것 말고도 저장된 자격 증명 정보를 사용할 수도 있습니다. 이 외에도 Alamofire를 사용하지 않고 URLSession을 사용하여도 HTTP 요청에 인증을 추가할 수 있습니다.

## 더 알아보기

- [Alamofire 공식 문서](https://github.com/Alamofire/Alamofire)
- [iOS 네트워킹 실제](https://www.raywenderlich.com/102-afnetworking-tutorial-getting-started) (영어)

## 같이 참고하기