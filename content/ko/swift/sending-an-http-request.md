---
title:                "http 요청 보내기"
html_title:           "Swift: http 요청 보내기"
simple_title:         "http 요청 보내기"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?  
HTTP 요청을 보내는 것은 무엇인지와 프로그래머들이 왜 그것을 하는지에 대해 알아보겠습니다. HTTP 요청이란 네트워크를 통해 서버에 데이터를 요청하는 것을 말합니다. 프로그래머들은 이를 통해 데이터를 받아와서 웹 앱을 만들거나 서드 파티 서비스와 연동할 수 있습니다.

## 하는 방법:
다음은 Swift를 사용하여 간단한 GET 요청을 보내는 예시입니다.

```Swift
// URL을 만들어줍니다.
let urlString = "https://swapi.dev/api/people/1/"
guard let url = URL(string: urlString) else { return }

// URL을 사용하여 URLRequest를 만들어줍니다.
let request = URLRequest(url: url)

// URLSession을 이용하여 요청을 보냅니다.
let task = URLSession.shared.dataTask(with: request) { (data, _, error) in
    // 요청이 성공하면 받아온 데이터를 출력합니다.
    if let data = data {
        print(String(data: data, encoding: .utf8))
    }
}

// 요청을 실행합니다.
task.resume()
```
**아웃풋:**
```Swift
Optional("{\"name\":\"Luke Skywalker\",\"height\":\"172\",\"mass\":\"77\",\"hair_color\":\"blond\",\"skin_color\":\"fair\",\"eye_color\":\"blue\",\"birth_year\":\"19BBY\",\"gender\":\"male\",\"homeworld\":\"https://swapi.dev/api/planets/1/\",\"films\":[\"https://swapi.dev/api/films/1/\"],\"species\":[\"https://swapi.dev/api/species/1/\"],\"vehicles\":[\"https://swapi.dev/api/vehicles/14/\"],\"starships\":[\"https://swapi.dev/api/starships/12/\"],\"created\":\"2014-12-09T13:50:51.644000Z\",\"edited\":\"2014-12-20T21:17:56.891000Z\",\"url\":\"https://swapi.dev/api/people/1/\"}")
```

## 깊게 들어가기:
HTTP 요청은 현재 인터넷에서 가장 많이 사용되는 프로토콜 중 하나입니다. 이 프로토콜을 통해 웹 페이지를 불러오거나 데이터를 주고받을 수 있습니다. 뿐만 아니라 Swift에서는 `URLSession`을 이용하여 이를 쉽게 구현할 수 있도록 지원하고 있습니다. 다른 대안으로는 `Alamofire`와 같은 서드 파티 라이브러리를 사용할 수도 있습니다.

## 관련 자료:
* [Apple 공식 문서 - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
* [Alamofire Git 저장소](https://github.com/Alamofire/Alamofire/)