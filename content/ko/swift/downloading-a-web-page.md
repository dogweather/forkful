---
title:                "웹 페이지 다운로드하기"
html_title:           "Arduino: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까? (What & Why?)
웹 페이지 다운로드란 인터넷에서 웹 페이지의 내용을 사용자의 컴퓨터나 장치에 복사하는 것을 의미합니다. 개발자들은 웹 페이지 분석, 자료 수집, 웹 크롤링 등을 위해 이를 실시합니다.

## 작성법 (How to:)
Swift에서 웹 페이지를 다운로드하는 간단한 코드는 아래와 같습니다:
```Swift
import Foundation

let url = URL(string: "http://example.com")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let data = data {
        let str = String(data: data, encoding: .utf8)
        print(str) 
    }
}
task.resume()
```
이 코드를 실행하면 "http://example.com" 웹 페이지의 HTML 을 문자열로 출력할 것입니다.

## 깊게 알아보기 (Deep Dive)
웹 페이지를 다운로드하기 위한 기술은 웹의 초기 시절부터 발전해 왔습니다. Swift에서는 URLSession을 이용한 위의 방법 외에도, Alamofire나 AFNetworking와 같은 서드 파티 라이브러리를 사용할 수도 있습니다. 하지만 URLSession은 내장된 라이브러리이므로 별도의 설치 없이 사용이 가능합니다. 또한, 그 성능과 안정성이 증명되어 있습니다.

## 참고자료 (See Also)
더 깊이 있는 지식을 얻고자 한다면 아래 링크를 참조하세요:
1. Apple의 공식 문서인 URLSession: https://developer.apple.com/documentation/foundation/urlsession
2. Swift에 대한 오라일리 북 ‘iOS 13 Programming Fundamentals with Swift’: https://www.oreilly.com/library/view/ios-13-programming/9781492074523/